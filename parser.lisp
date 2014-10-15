(in-package #:vertigo)

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited)) &optional args)
  (destructuring-bind (delimiter &body body) args
    (meta-sexp:transform-grammar ret ctx t :checkpoint (join-list delimiter body))))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :delimited*)) &optional args)
  (destructuring-bind (delimiter &body items) args
    (meta-sexp:transform-grammar ret ctx t :checkpoint
                                 `(,@items
                                   (:* (:checkpoint ,delimiter
                                                    ,@items))))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :char)) &optional args)
  (destructuring-bind (char-exp) args
    (let ((char-var (gensym "CHAR")))
      `(let (,char-var)
         ,(meta-sexp:transform-grammar ret ctx t :checkpoint
          `((:assign ,char-var (:type character))
            (eql ,char-var ,char-exp)
            ,char-var))))))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :cursor)) &optional args)
  (declare (ignore args))
  `(meta-sexp:cursor ,ctx))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :data)) &optional args)
  (declare (ignore args))
  `(meta-sexp:context-data ,ctx))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :subseq)) &optional args)
  (destructuring-bind (start &optional end) args
    `(meta-sexp:context-subseq ,ctx
                               ,(meta-sexp:transform-grammar ret ctx t start)
                               ,(meta-sexp:transform-grammar ret ctx t end))))

;; TODO: Probably need checkpointing in here
(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :with-stored-match)) &optional args)
  (destructuring-bind ((place) &body body) args
    (let* ((index-var (gensym "INDEX"))
           (result-var (gensym "RESULT"))
           (match-save (meta-sexp:transform-grammar
                        ret ctx t :assign
                        (list place
                              (meta-sexp:transform-grammar ret ctx t :subseq (list index-var '(:cursor)))))))
      `(let ((,index-var ,(meta-sexp:transform-grammar ret ctx t :cursor))
             (,result-var ,(meta-sexp:transform-grammar ret ctx t :and body)))
         ;; (:and ,@body (:assign var (:matched-since index)))
         ,(meta-sexp:transform-grammar ret ctx t :and
                                       (list result-var
                                             match-save
                                             ;; Want to return this as
                                             ;; the value of the :and
                                             result-var))))))

(defmethod meta-sexp:transform-grammar (ret ctx (in-meta (eql t)) (directive (eql :n-times)) &optional args)
  (let ((count (first args)))
    (meta-sexp:transform-grammar ret ctx t :and (loop for i from 1 to count
                                                   append (cdr args)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-binds)) &optional args)
  (let ((*bind-vars* '()))
    (declare (special *bind-vars*))
    ;; Reserve first arg for future arglist (e.g (:with-binds () ...))
    (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint
                                                  (cdr args))))
      `(let ,(reverse *bind-vars*)
         ,body-code))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :bind)) &optional args)
  (declare (special *bind-vars*))
  (when (not (boundp '*bind-vars*))
    (error ":BIND outside of :WITH-BINDS form"))
  (flet ((var-spec-var (spec) (if (consp spec) (first spec) spec)))
    (destructuring-bind (form var-spec &aux (var (var-spec-var var-spec))) args
      (restart-case (progn
                      (when (member var *bind-vars*
                                    :key #'var-spec-var)
                        (error "Variable ~S is already bound by a :BIND form~%" var))
                      (push var-spec *bind-vars*)
                      (meta-sexp:transform-grammar ret ctx t :assign
                                                   (list var form)))
        (use-value (new-var-spec)
          :report "Use a different variable spec"
          :interactive (lambda ()
                         (prompt-until "Enter a new variable spec"
                                       (lambda (s)
                                         (let ((primary-value (first s)))
                                           ;; True if either it's a
                                           ;; var or a (var binding)
                                           ;; form
                                           (or (symbolp primary-value)
                                               (and (listp primary-value)
                                                    (symbolp (first primary-value))
                                                    (endp (cddr primary-value)))
                                               (format *query-io* "~&Not a valid variable spec, try again.~%"))))))

          (setf var-spec new-var-spec))))))

;; TODO: the actual dict appears to be lexically scoped; is this a
;; problem for e.g. cross-rule binds?
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-dict-binds)) &optional args)
  (let ((*dict-var* (gensym "DICT")))
    (declare (special *dict-var*))
    ;; First arg is reserved for future arguments
    (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint (cdr args))))
      `(let ((,*dict-var* (make-instance 'cl-persist:persistent-map)))
         (and ,body-code
              ,*dict-var*)))))

;; TODO: :bind/:kbind checkpointing (only do assigns if all
;; checkpointed forms succeed.
;; TODO: what about if/when a key is non-literal? *bind-keys* probably
;; breaks, ',key stops being legit...
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :kbind)) &optional args)
  (declare (special *dict-var*))
  (when (not (boundp '*dict-var*))
    (error ":KBIND outside of :WITH-DICT-BINDS form"))
  (destructuring-bind (form key &optional (key-func (quote #'identity))) args
    (restart-case (progn
                    (let ((form-code (meta-sexp:transform-grammar ret ctx t form))
                          (result-var (gensym "RESULT")))
                      `(let ((,result-var ,form-code))
                         (when ,result-var
                           (setf ,*dict-var*
                                 (cl-persist:add ,*dict-var* ',key (funcall ,key-func ,result-var)))
                           ,result-var))))
        (use-value (new-key)
          :report "Use a different key"
          :interactive (lambda ()
                         (prompt-for-val "Enter a new key"))
          (setf key new-key)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :dict-checkpoint)) &optional args)
  (declare (special *dict-var*))
  (when (not (boundp '*dict-var*))
    (error ":DICT-CHECKPOINT outside of :WITH-DICT-BINDS form"))
  (let ((stored-dict-var (gensym "STORED-DICT"))
        (result-var (gensym "RESULT"))
        (body-code (meta-sexp:transform-grammar ret ctx t :and args)))
    `(let ((,stored-dict-var ,*dict-var*))
       (let ((,result-var ,body-code))
         (unless ,result-var
           (setf ,*dict-var* ,stored-dict-var))
         ,result-var))))

;; Combined dict/parser checkpoint
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :checkpoint*)) &optional args)
  (declare (special *dict-var*))
  (meta-sexp:transform-grammar ret ctx t :checkpoint
                               (if (boundp '*dict-var*)
                                   (list (cons :dict-checkpoint args))
                                   args)))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :key-flags)) &optional args)
  (meta-sexp:transform-grammar ret ctx t :and
                               (loop for spec in args
                                  collect (if (listp spec)
                                              `(:opt (:kbind,(first spec) ,(second spec) (constantly t)))
                                              `(:opt (:kbind ,spec ,spec (constantly t)))))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :opt)) &optional args)
  (if (list-length-p 1 args)
      ;; We assume a singleton form doesn't any needed checkpointing
      (meta-sexp:transform-grammar ret ctx t :? args)
      (meta-sexp:transform-grammar ret ctx t :? (list (cons :checkpoint* args)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :k)) &optional args)
  (if (list-length-p 1 args)
      ;; Single argument doesn't need an extra checkpoint
      (meta-sexp:transform-grammar ret ctx t :rule
                                   (cons 'keyword? args))
      (meta-sexp:transform-grammar ret ctx t :checkpoint
                                   (mapcar (lambda (form)
                                             `(:rule keyword? ,form))
                                           args))))

;; Match a keyword as a :k form
(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive symbol) &optional args)
  "Transforms a symbol form (only keywords are transformed, other symbols are left alone)."
  (declare (ignore args))
  (if (keywordp directive)
      (meta-sexp:transform-grammar ret ctx t :k (list (symbol-name directive)))
      directive))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :whole-match)) &optional args)
  "Transform for matching a form that must consume all remaining input."
  (let ((body-code (meta-sexp:transform-grammar ret ctx t :checkpoint args))
        (post-code (meta-sexp:transform-grammar ret ctx t :eof '()))
        (result-var (gensym "RESULT")))
    `(let ((,result-var ,body-code))
       (and ,post-code ,result-var))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-context-for)) &optional args)
  (destructuring-bind ((context-data) &rest forms) args
    (let ((ctx-var (gensym "CONTEXT")))
      `(let ((,ctx-var (meta-sexp:create-parser-context ,context-data)))
         ,(meta-sexp:transform-grammar ret ctx-var t :checkpoint forms)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :with-context)) &optional args)
  (destructuring-bind ((context) &rest forms) args
    (let ((ctx-var (gensym "CONTEXT")))
      `(let ((,ctx-var ,context))
         ,(meta-sexp:transform-grammar ret ctx-var t :checkpoint forms)))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :peek-atom)) &optional args)
  (declare (ignore args))
  `(meta-sexp:peek-atom ,ctx))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :context)) &optional args)
  (declare (ignore args))
  ctx)

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :commit)) &optional args)
  (declare (ignore args))
  `(progn (meta-sexp:commit ,ctx)
          t))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :optima)) &optional args)
  (let ((obj-var (gensym "OBJ"))
        (eof-var (gensym "EOF-P")))
    (meta-sexp:transform-grammar ret ctx t :checkpoint
                                 `((multiple-value-bind (,obj-var ,eof-var) (meta-sexp:read-atom ,ctx)
                                     (if ,eof-var
                                         nil
                                         (optima:match ,obj-var
                                           ,@args)))))))

(defmethod meta-sexp:transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :any-prefix)) &optional args)
  (let ((accum-var (gensym "ACCUM")))
    `(let ((,accum-var (meta-sexp:make-char-accum)))
       ,(meta-sexp:transform-grammar ret ctx t :and
                                     `((:? ,@(loop for c across (first args)
                                                collect c
                                                collect `(:char-push ,c ,accum-var)))
                                      ,accum-var)))))

(deftype whitespace-char ()
  '(or (eql #\Tab)
    (eql #\Newline)
    (eql #\Linefeed)
    (eql #\Page)
    (eql #\Return)
    (eql #\Space)))

;; TODO: Not all of these character names are portable, use codes for
;; everything but #\Newline and #\Space
(meta-sexp:defrule whitespace? () ()
  (:+ (:type whitespace-char)))
