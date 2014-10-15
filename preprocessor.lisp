(in-package :vertigo)

;;; Stuff for special parser contexts

(defclass text-position-mixin ()
  ;; Interesting note: if this is initialized to 0, then line-num is
  ;; the number of unix-style lines we've read (i.e. "it's not a line
  ;; unless it ends with a newline", a la ed(1)).
  ((line :accessor line-num :initarg :line)
   (column :accessor col-num :initarg :column)
   (line-ending-style :accessor eol-style :initarg :eol-style)
   (carriage-return-p :accessor returnp :initform nil)
   (in-newline-p :accessor newlinep :initform nil)
   (checkpoint-stack :accessor checkpoints :initform nil))
  (:default-initargs :eol-style :unix
    :line 1
    :column 0))

;; The spec:

;; 1. Every character (including newlines) increments the column (CRLF
;; counts as one character, and doesn't increment until both
;; characters have been read).

;; 2. If the last character read was a newline, the next character
;; read (including newlines) will be at the next line number, column
;; 1.
(defgeneric process-char (context char)
  (:documentation "Process the CHAR, updating file position state as necessary.")
  (:method ((context text-position-mixin) char)
    (check-type char character)
    (check-type (eol-style context) (member :unix :dos :both))
    (let* ((mode (eol-style context))
           (crlfp (member mode '(:dos :both))))
      ;; Before we do anything else, CR followed by LF is a NOP
      ;; character, so just set returnp and bail.
      (when (and crlfp
                 (eql char #\Return)
                 (eql (meta-sexp:peek-atom context) #\Linefeed))
        (setf (returnp context) t)
        (return-from process-char (values)))

      ;; If we just saw a newline, increment the line number and make
      ;; sure the column increment will land at 1. Also reset the
      ;; newline state (it will be re-set if necessary)
      (when (newlinep context)
        (setf (col-num context) 0
              (newlinep context) nil)
        (incf (line-num context)))
      ;; Always increment
      (incf (col-num context))
      ;; Now see if /this/ character is a newline
      (when (and (eql char #\Linefeed)
                 (or (not (eq mode :dos))
                     (returnp context)))
        (setf (newlinep context) t))

      ;; Anything other than an \r\n clears returnp
      (setf (returnp context) nil))))


(defmethod meta-sexp:read-atom :around ((ctx text-position-mixin))
  (multiple-value-bind (atom eofp) (call-next-method)
    (unless eofp
      (process-char ctx atom))
    (values atom eofp)))

(defmethod meta-sexp:checkpoint :after ((ctx text-position-mixin))
  (push (list (line-num ctx)
              (col-num ctx)
              (returnp ctx)
              (newlinep ctx))
        (checkpoints ctx)))

(defmethod meta-sexp:commit :after ((ctx text-position-mixin))
  (pop (checkpoints ctx)))

(defmethod meta-sexp:rollback :after ((ctx text-position-mixin))
  (destructuring-bind (line col ret newline) (first (checkpoints ctx))
    (setf (line-num ctx) line
          (col-num ctx) col
          (returnp ctx) ret
          (newlinep ctx) newline)))

(defclass counting-string-context (meta-sexp::string-parser-context text-position-mixin)
  ())

;; We want to use counting-string-context by default for strings
(defmethod meta-sexp:create-parser-context :around
    ((input string) &key attachment (end (length input)) (start 0) (line 1) (column 0) (eol-style :both))
  (make-instance 'counting-string-context
                 :data input
                 :attachment attachment
                 :start start
                 :size end
                 :attachment attachment
                 :eol-style eol-style
                 :column column
                 :line line
                 :column column))

(defun test-context (mode str)
  (let ((context (make-instance 'counting-string-context
                                :data str
                                :start 0
                                :size (length str)
                                :eol-style mode)))
    (loop for atom = (meta-sexp:read-atom context)
       while atom)
    (list :line (line-num context) :col (col-num context) :returnp (returnp context) :newlinep (newlinep context))))

(defclass preprocessor-string-context (counting-string-context)
  ((global-environment :accessor global-env :initform '())
   (scoped-environment :accessor scoped-env :initform '(()))
   (expansion-context :accessor expansion-context :initform nil)))

(defgeneric add-preproc-scope (context)
  (:documentation "Add a new processor scope to CONTEXT.")
  (:method ((context preprocessor-string-context))
    (push nil (scoped-env context))
    (values)))

(defgeneric close-preproc-scope (context)
  (:documentation "Close (remove) the current processor scope in CONTEXT.")
  (:method ((context preprocessor-string-context))
    (when (scoped-env context)
      (pop (scoped-env context)))
    (values)))

;; TODO: it's probably not worth using this over pushnew/acons. Test
;; on actual file.
(defun substitute-assoc (key value alist &key (test #'eql))
  "Associate VALUE with KEY in the alist ALIST, but with less consing than ACONS."
  (labels ((replace-item (list)
             (cond
               ((null list) nil)
               ((and (first list)
                     (funcall test key (car (first list))))
                ;; found it, replace the value and the rest of the list
                (cons (cons (car (first list))
                            value)
                      (cdr list)))
               (t (let ((rest (replace-item (cdr list))))
                    (if (eq rest (cdr list))
                        ;; No changes, return the current list
                        list
                        ;; Otherwise cons the current element
                        (cons (first list) rest)))))))
    (let ((list (replace-item alist)))
      (if (eq list alist)
          ;; No existing element, cons it on
          (acons key value alist)
          ;; Otherwise done
          list))))

(defgeneric globally-define (context symbol text)
  (:documentation "Define the preprocessor SYMBOL to have the replacement text TEXT globally.")
  (:method ((context preprocessor-string-context) symbol text)
    (check-type symbol string)
    (check-type text string)
    ;; It would be more memory-efficient to use substitute-assoc, but
    ;; that's a lot of extra code.
    (push (cons symbol text) (global-env context))
    (values)))

;; TODO: this needs heavy testing for checkpointing
(defgeneric locally-define (context symbol text)
  (:documentation "Define the preprocessor SYMBOL to have the replace text TEXT within the current scope.")
  (:method ((context preprocessor-string-context) symbol text)
    (check-type symbol string)
    (check-type text string)
    (assert (not (endp (scoped-env context))) ()
            "There is no scope active for this definition.")
    ;; (push <..> (first (scoped-env ctx))) is not safe, since
    ;; (setf (first ..)) will destructively modify the env.
     (let* ((old-env (scoped-env context))
            (new-env (cons (acons symbol text (first old-env))
                           (rest old-env))))
       (setf (scoped-env context) new-env))
     (values)))

;; TODO: add support for builtins like OPSYS and the {n}/{*} specials
(defgeneric symbol-text (context symbol)
  (:documentation "Return the current replacement text for the symbol SYMBOL, or NIL if SYMBOL is currently undefined. The secondary value is T if the currently active definition of SYMBOL is global, NIL otherwise.")
  (:method ((context preprocessor-string-context) symbol)
    (check-type symbol string)
    ;; preprocessor symbols are case-insensitive.
    (let ((local-def (assoc symbol (first (scoped-env context)) :test #'string-equal))
          (global-def (assoc symbol (global-env context) :test #'string-equal)))
      (cond
        (local-def
         (values (cdr local-def) nil))
        (global-def
         (values (cdr local-def) t))
        (t (values nil nil))))))

(defgeneric undefine-symbol (context symbol)
  (:documentation "Remove the currently-active definition of SYMBOL in CONTEXT.")
  (:method ((context preprocessor-string-context) symbol)
    (check-type symbol string)
    (multiple-value-bind (text global-p) (symbol-text context symbol)
      ;; Note that definitions don't stack, so removing all
      ;; definitions from the appropriate scope is correct.
      (cond
        ;; Global definition in effect
        ((and text global-p)
         (setf (global-env context) (remove symbol (global-env context)
                                            :test #'string-equal
                                            :key #'car)))
        ;; Local definition
        (text (let* ((old-env (first (scoped-env context)))
                     (new-env (cons (remove symbol (first old-env)
                                            :test #'string-equal
                                            :key #'car)
                                    (rest old-env))))
                (setf (scoped-env context) new-env)))))
    (values)))

;; TODO: get/report the position of the preproc symbol on success
(meta-sexp:defrule preproc-symbol? (&aux match) ()
  (:or (:checkpoint "{&"
                    (:with-stored-match (match)
                      (:* (:not #\})
                          (:type character)))
                    "}")
       (:checkpoint "{"
                    (:with-stored-match (match)
                      (:or #\*
                           (:+ (:type meta-sexp:digit?))))
                    "}"))
  (list :symbol match))

(meta-sexp:defrule named-include-param? (&aux name value) ()
  (:checkpoint #\&
               (:or (:assign name (:rule parse-quoted-chars #\'))
                    (:assign name (:rule parse-quoted-chars #\"))
                    (:with-stored-match (name)
                      (:* (:and (:not (:type whitespace-char))
                                (:not (:or #\' #\"))
                                (:not #\=)
                                (:type character)))))
               (:? (:rule whitespace?))
               #\=
               (:? (:rule whitespace?))
               (:or (:assign value (:rule parse-quoted-chars #\'))
                    (:assign value (:rule parse-quoted-chars #\"))
                    (:with-stored-match (value)
                      (:+ (:and (:not (:type whitespace-char))
                                (:not (:or #\' #\"))
                                (:not #\})
                                (:type character))))))
  (if (and name value)
      (list name value)
      nil))

(meta-sexp:defrule positional-include-param? (&aux value) ()
  (:or (:assign value (:rule parse-quoted-chars #\'))
       (:assign value (:rule parse-quoted-chars #\"))
       (:with-stored-match (value)
         (:+ (:and (:not (:type whitespace-char))
                   (:not (:or #\' #\"))
                   (:not #\})
                   (:type character)))))
  value)

(meta-sexp:defrule include-params? (&aux (params '()) param) ()
  ;; Positional params must come before named params, and take care
  ;; not to confuse the two.
  (:+ (:assign param (:or (:rule named-include-param?)
                          (:rule positional-include-param?)))
      (:list-push param params)
      (:? (:rule whitespace?)))
  (make-list-box :list (nreverse params)))

;; NOTE: includes expand to the file contents, plus a newline and a space.
;; TODO: ABL's quoting stuff for include file names is broken; do we
;; need to duplicate it?
(meta-sexp:defrule preproc-include? (&aux match params) ()
  (:with-stored-match (match)
    #\{
    (:? (:rule whitespace?))
    (:assign params (:rule include-params?))
    (:? (:rule whitespace?))
    #\})
  (cons :include (list-box-list params)))

(meta-sexp:defrule preprocessor-block? (&aux match) ()
  (:or (:rule preproc-symbol?)
       (:rule preproc-include?)
       (:and (:with-stored-match (match)
               (:+ (:or (:rule match-comment)
                        ;; preproc stuff starts with a #\{ (yeah, I
                        ;; know, magic values, assumptions, etc.)
                        (:and (:not #\{)
                              (:type character)))))
             (list :text match))))

;; NOTE: according to the manual, this is only valid to use at the
;; beginning of a new line.
(meta-sexp:defrule preproc-command? (&aux (texts '()) text) ()
  ;; initial whitespace and comments preserved
  (:with-stored-match (text)
    (:* (:or (:rule whitespace?)
             (:rule match-comment))))
  (:list-push text texts)
  ;; The actual command (with possible abbreviations
  (:or (:icase (:and "&scop" (:any-prefix "ed-define")))
       (:icase "&undef" (:any-prefix "ine"))
       (:icase "&globa" (:any-prefix "l-define"))
       ;; AppBuilder magic
       (:icase "&analyze-s" (:any-prefix "uspend"))
       (:icase "&analyze-r" (:any-prefix "esume")))
  texts)
