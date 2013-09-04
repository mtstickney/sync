;;;; install-gen.lisp

(in-package #:install-gen)

;;; "install-gen" goes here. Hacks and glory await!

(defvar *defs* (make-hash-table :test 'equal))
(defvar *effects* (make-hash-table :test 'equal))

(define-condition missing-file-error (error)
  ((path :initarg :path
         :reader missing-file-path)
   (type :initarg :type
         :reader missing-file-type)
   (use :initarg :use
        :initform nil
        :reader missing-file-use))
  (:documentation "Condition signalled when a required file is missing.")
  (:report (lambda (condition stream)
             (format stream "Required ~A ~S is missing"
                     (missing-file-type condition)
                     (missing-file-path condition))
             (if (missing-file-use condition)
                 (format stream ", ~A." (missing-file-use condition))
                 (format stream "."))
             (values))))

(define-condition abl-error (error)
  ((errors :initarg :errors
           :reader abl-error-errors))
  (:documentation "Condition signalled to report errors from a callout to ABL.")
  (:report (lambda (condition stream)
             (format stream "~@<ABL returned with errors:~@:_~I~{~<~A: ~:_~I~W~@:_~:>~}~:>"
                     (abl-error-errors condition)))))

(define-condition java-error (error)
  ((errors :initarg :errors
           :reader java-error-errors))
  (:documentation "Condition signalled to report errors from a callout to Java.")
  (:report (lambda (condition stream)
             (format stream "~@<Java returned with errors:~@:_~I~{~<~A: ~:_~I~W~@:_~:>~}~:>"
                     (java-error-errors condition)))))

(defun require-file (path type &optional use)
  (check-type path (or string pathname))
  (unless (probe-file path)
    (error 'missing-file-error
           :path path
           :type type
           :use use)))

(defun path-getter (path type)
  (check-type path (or string pathname))
  (assert (cl-fad:pathname-relative-p path) (path)
          "Path ~S is not relative" path)
  (macrolet ((fetcher (base-symb)
               `(lambda ()
                  (declare (special ,base-symb))
                  (check-type ,base-symb (or string pathname))
                  (if (cl-fad:directory-pathname-p path)
                      (cl-fad:merge-pathnames-as-directory ,base-symb path)
                      (cl-fad:merge-pathnames-as-file ,base-symb path)))))
    ;; TODO: PROBE-FILE :src and :aux (but then we can't
    ;; build the code without having all the resources...)
    (ecase type
      (:src (fetcher *src-dir*))
      (:dest (fetcher *dest-dir*))
      (:aux (fetcher *res-dir*))
      ;; Note: exists for things that are generated, bypasses
      ;; (future) check during build.
      (:gen (fetcher *res-dir*)))))

;; TODO: Maybe want a higher-level "ignore-missing" restart for
;; multiple files
(defun remove-file (path)
  (declare (special *ignore-missing-deletes*))
  (check-type path (or string pathname))
  (restart-case (delete-file path)
    (skip-file ()
      :report "Ignore the error and skip the file."
      (return-from remove-file (values))))
  (values))

;; Note: the use of TRUENAME fixes an issue with ENOUGH-NAMESTRING
;; when the case of FROM doesn't match the case on the filesystem
(defun copy-directory (from to &key overwrite)
  (declare (special *file-callback*))
  (check-type from (or string pathname))
  (check-type to (or string pathname))
  (format *debug-io* "Copying directory ~S to ~S~%" from to)
  (let ((from (truename (cl-fad:pathname-as-directory from)))
        (to (cl-fad:pathname-as-directory to)))
    (labels ((dest-path (path)
               (let ((postfix (enough-namestring (truename path) from)))
                 (merge-pathnames (if (cl-fad:directory-pathname-p path)
                                      (cl-fad:pathname-as-directory postfix)
                                      (cl-fad:pathname-as-file postfix))
                                  to)))
             (copy-item (item)
               (when (boundp '*file-callback*)
                 (funcall *file-callback* item))
               (format *debug-io* "Supposedly copying ~S~%" item)
               (let ((dest (dest-path item)))
                 (if (cl-fad:directory-pathname-p item)
                     (ensure-directories-exist dest)
                     (progn
                       (ensure-directories-exist (cl-fad:pathname-directory-pathname item))
                       (cl-fad:copy-file item dest :overwrite overwrite))))))
      (cl-fad:walk-directory from (lambda (p)
                                    (unless (equal p from)
                                      (copy-item p)))
                             :directories :breadth-first)
      (values))))

(defun tree-diff-applicator (cmd path)
  (check-type cmd keyword)
  (check-type path (or pathname string))
  (let ((src-pather (path-getter path :src))
        (dest-pather (path-getter path :dest)))
    (ecase cmd
      (:+ (lambda ()
            (let ((src-path (funcall src-pather))
                  (dest-path (funcall dest-pather)))
              (if (cl-fad:directory-pathname-p src-path)
                  (copy-directory src-path dest-path :overwrite t)
                  (cl-fad:copy-file src-path dest-path :overwrite t)))))
      (:- (lambda ()
            (let ((dest-path (funcall dest-pather)))
              (if (cl-fad:directory-pathname-p dest-path)
                  (cl-fad:delete-directory-and-files dest-path)
                  (remove-file (funcall dest-pather)))))))))

(defun tree-application (tree-spec)
  (check-type tree-spec list)
  (let ((applicators (mapcar (lambda (file-spec)
                               (apply #'tree-diff-applicator file-spec))
                             tree-spec)))
    ;; TODO: Add a top-level tree application restart
    (lambda ()
      (loop for app in applicators
         do (funcall app)))))

(defun progress-bin (binary-path)
  (declare (special *progress-dir*))
  (check-type binary-path (or string pathname))
  (let ((path (cl-fad:pathname-as-file binary-path)))
    (assert (cl-fad:pathname-relative-p path) (path)
            "~S is not a relative pathname." path)
    (merge-pathnames path *progress-dir*)))

(defun read-abl-result (file)
  ;; TODO: use cl-secure-read for this
  (let ((result (with-open-file (fh file)
                  (read fh))))
    (check-type result list)
    (case (car result)
      (:data (apply #'values (cdr result)))
      (:error (error 'abl-error :errors (cdr result)))
      (t (error 'abl-error :errors (list "ABL process failed to return legible result."))))))

;; TODO: make sure to handle EOF errors
(defun run-abl (proc output-file system-args &rest args)
  (declare (special *progress-dir*))
  (check-type *progress-dir* (or string pathname))
  (labels ((prowin-args (proc system-args args)
             (concatenate 'list
                          (list "-p" proc
                                "-param" (format nil "~{~A~^,~}" args))
                          system-args)))
    (let* ((prowin-path (let ((path (progress-bin "bin/prowin32.exe")))
                          (require-file path "Progress binary" "unable to run ABL procedure")
                          path)))
      (external-program:run
                      prowin-path
                      (prowin-args proc system-args args))
      (if output-file
          (read-abl-result output-file)
          (values)))))

(defun run-java (class &key classpath defs args)
  (declare (special *progress-dir* *db-file*))
  (flet ((java-args (class classpath defs args)
           (concatenate 'list
                        (if classpath
                            (list "-cp"
                                  ;; Windows classpath separator is ';'
                                  (format nil "~{~A~^;~}" classpath))
                            '())
                        (loop for def in defs
                           collect
                             (destructuring-bind (term val) def
                               (format nil "-D~A=~A" term val)))
                        (list class)
                        args)))
    (let* ((comm-file (mk-temp-file "jcomm"))
           (result (unwind-protect
                        (progn
                          (external-program:run (funcall (path-getter #P"jre7/bin/java.exe" :aux))
                                                (java-args class classpath defs (cons comm-file args)))
                          (with-open-file (fh comm-file)
                            (read fh nil :eof)))
                     (delete-file comm-file))))
      (if (and (listp result)
               (keywordp (car result))
               (or (eql (car result) :data)
                   (eql (car result) :error)))
          (case (car result)
            (:data (cdr result))
            (:error (error 'java-error :errors (cdr result))))
          (error 'java-error :errors (list "Java process failed to return legible result."))))))

(defmacro defeffect (name args &body body)
  (check-type args list)
  (check-type name symbol)
  `(setf (gethash (symbol-name ,name) *effects*)
         (lambda ,args ,@body)))

;;; FFI stuff for registry key business
(cffi:define-foreign-library advapi32
  (t (:default "advapi32")))

(cffi:use-foreign-library advapi32)

(cffi:defcenum (reg-key-access :ulong)
  (:all-access #xf003f)
  (:create-link #x0020)
  (:create-sub-key #x0004)
  (:enumerate-sub-keys #x0008)
  (:execute #x20019)
  (:notify #x0010)
  (:query-value #x0001)
  (:read #x20019)
  (:set-value #x0002)
  (:wow64-32key #x0200)
  (:wow64-64key #x0100)
  (:key-write #x20006))

(cffi:defcfun (reg-open-key-ex "RegOpenKeyExA") :long
  (key :pointer)
  (subkey :pointer)
  (options :ulong)
  (regsam :ulong)
  (result :pointer))

(cffi:defcfun (reg-query-value-ex "RegQueryValueExA") :long
  (key :pointer)
  (subkey :pointer)
  (reserved :pointer)
  (type :pointer)
  (data :pointer)
  (data-size :pointer))

(cffi:defcfun (reg-close-key "RegCloseKey") :long
  (key :pointer))

(defun & (addr)
  (check-type addr unsigned-byte)
  (cffi:make-pointer addr))

(define-symbol-macro hkey-classes-root (& #x80000000))
(define-symbol-macro hkey-current-user (& #x80000001))
(define-symbol-macro hkey-local-machine (& #x80000002))
(define-symbol-macro hkey-users (& #x80000003))
(define-symbol-macro hkey-performance-data (& #x80000004))
(define-symbol-macro hkey-performance-text (& #x80000050))
(define-symbol-macro hkey-performance-nls-text (& #x80000060))
(define-symbol-macro hkey-current-config (& #x80000005))
(define-symbol-macro hkey-dyn-data (& #x80000006))

(cffi:defcenum (registry-value-type :ulong)
  :none
  :sz
  :expand-sz
  :binary
  :dword
  :dword-big-endian
  :link
  :multi-sz
  :resource-list
  :full-resource-descriptor
  :resource-requirements-list
  :qword)

(cffi:defcenum (return-code :long)
  (:success 0)
  (:more-data #xEA))

(define-condition foreign-system-error (error)
  ((datum :initarg :datum
          :reader sys-error-datum)
   (args :initarg :args
         :reader sys-error-args)
   (code :initarg :code
         :reader sys-error-code))
  (:report (lambda (c s)
             (format s "System error, code ~S: " (sys-error-code c))
             (apply #'format s (sys-error-datum c)
                    (sys-error-args c)))))

(defun system-error (code datum &rest args)
  (error 'foreign-system-error
         :code code
         :datum datum
         :args args))

(defgeneric reg-to-lisp (type data size)
  (:documentation "Convert the data from querying a registry key's value to a ~
lisp type. TYPE, DATA, and SIZE are those reported by RegQueryValueEx.")
  (:method ((type (eql :sz)) data size)
    (cffi:foreign-string-to-lisp data :max-chars size)))

(defun reg-read-val (base-key subkey value-name)
  (cffi:with-foreign-strings ((subkey-str subkey)
                              (value-str value-name))
    (cffi:with-foreign-objects ((key :pointer)
                                (type 'registry-value-type)
                                (data-size :ulong))
      (let ((ret (reg-open-key-ex hkey-local-machine
                                  subkey-str
                                  0
                                  (cffi:foreign-enum-value 'reg-key-access :query-value)
                                  key)))
        (unless (zerop ret)
          (system-error ret "Error opening registry key ~S~%"
                        (format nil "~A\\~A" base-key subkey)))
        (unwind-protect
             (progn
               ;; Get the size of the buffer
               (setf (cffi:mem-aref data-size :ulong) 0)
               (setf ret (reg-query-value-ex
                          (cffi:mem-aref key :pointer)
                          value-str
                          (cffi:null-pointer)
                          type
                          (cffi:null-pointer)
                          data-size))
               (unless (or (zerop ret)
                           (eq ret (cffi:foreign-enum-value 'return-code :more-data)))
                 (system-error ret "Error querying value ~S for key ~S~%"
                               value-name
                               (format nil "~A\\~A" base-key subkey)))
               (cffi:with-foreign-pointer (data (cffi:mem-aref data-size :ulong))
                 (setf ret (reg-query-value-ex
                            (cffi:mem-aref key :pointer)
                            value-str
                            (cffi:null-pointer)
                            type
                            data
                            data-size))
                 (unless (zerop ret)
                   (system-error ret "Error retrieving value ~S for key ~S~%"
                                 value-name
                                 (format nil "~A\\~A" base-key subkey)))
                 (return-from reg-read-val (values (reg-to-lisp (cffi:mem-aref type 'registry-value-type)
                                                                data
                                                                (cffi:mem-aref data-size :ulong))
                                                   (cffi:mem-aref type 'registry-value-type)))))
          (reg-close-key (cffi:mem-aref key :pointer)))))))

(defun reg-key-exists-p (main-key subkey)
  (check-type subkey string)
  (assert (cffi:pointerp main-key) (main-key)
          "Main key ~S is not a pointer."
          main-key)
  (cffi:with-foreign-string (subkey-str subkey)
    (cffi:with-foreign-object (key :pointer)
      (let ((ret (reg-open-key-ex main-key
                                  subkey-str
                                  0
                                  (cffi:foreign-enum-value 'reg-key-access :query-value)
                                  key)))
        (if (zerop ret)
            (progn
              (reg-close-key (cffi:mem-aref key :pointer))
              t)
            nil)))))

(defun reg-value-exists-p (main-key subkey value)
  "Return T if SUBKEY is a subkey of MAIN-KEY and has a value named VALUE. Returns nil if there was an error querying the value."
  (check-type subkey string)
  (check-type value string)
  (assert (cffi:pointerp main-key) (main-key)
          "Main key ~S is not a pointer."
          main-key)
  (handler-case (reg-read-val main-key subkey value)
    (foreign-system-error (c)
      (declare (ignore c))
      (return-from reg-value-exists-p nil))))

(defun product-subkey (product-code)
  ;; Not sure whether to take the code with or without the surrounding
  ;; {}s, so try to do both.
  (let ((code (if (eql (elt product-code 0) #\{)
                  product-code
                  (format nil "{~A}" product-code))))
    (format nil "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\~A" code)))

(defun product-installed-p (product-code)
  "Returns T if the product with the GUID PRODUCT-CODE is installed on the machine, NIL otherwise (this is true if the program appears in the 'Add/Remove Programs' listing."
  (let ((subkey (product-subkey product-code)))
    (reg-key-exists-p hkey-local-machine subkey)))

(defun product-install-dir (product-code)
  (let ((subkey (product-subkey product-code)))
    (reg-value-exists-p hkey-local-machine subkey "InstallLocation")))

(defun env-value (var)
  (check-type var string)
  #+clisp (ext:getenv var)
  #+sbcl (sb-posix:getenv var)
  #+ccl (ccl:getenv var)
  #-(or clisp sbcl ccl) (error "ENV-VALUE is unsupported on this Lisp."))

(defun effects-applicator (effects)
  (let ((applicators (loop for e in effects
                        collect (if (not (listp e))
                                    (funcall (find-effect e))
                                    (apply (find-effect (car e))
                                           (mapcar #'eval (cdr e)))))))
    ;; TODO: Add some facility to outputting status messages while
    ;; these things are happening
    (lambda ()
      (loop for applicator in applicators
         do (funcall applicator)))))

(defun make-installer (def)
  (check-type def list)
  (let ((pre-effects (assoc :pre-effects def))
        (post-effects (assoc :post-effects def))
        (tree-spec (assoc :tree def))
        pre-applicator
        post-applicator
        tree-applicator)
    (when pre-effects
      (setf pre-applicator (effects-applicator (cdr pre-effects))))
    (when post-effects
      (setf post-applicator (effects-applicator (cdr post-effects))))
    (when tree-spec
      (setf tree-applicator (tree-application (cdr tree-spec))))
    (lambda ()
      (when pre-applicator
        (funcall pre-applicator))
      (when tree-applicator
        (funcall tree-applicator))
      (when post-applicator
        (funcall post-applicator)))))

(deftype installer-designator () '(or symbol string function))

(defun find-installer (installer-designator)
  (check-type installer-designator installer-designator)
  (etypecase installer-designator
    (function installer-designator)
    (symbol (gethash (symbol-name installer-designator) *defs*))
    (string (gethash installer-designator *defs*))))

(defun find-effect (effect-designator)
  (check-type effect-designator (or symbol string function))
  (etypecase effect-designator
    (function effect-designator)
    (symbol (gethash (symbol-name effect-designator) *effects*))
    (string (gethash effect-designator *effects*))))

(defmacro definstaller (name &body body)
  (check-type name symbol)
  (let* ((probes (cdr (assoc :probes body)))
         (probe-vars (mapcar #'first probes))
         (def-var (gensym "DEF"))
         (installer-var (gensym "INSTALLER"))
         (probe-val-var (gensym "PROBE")))
    `(setf (gethash ,(symbol-name name) *defs*)
           (let* ((,def-var (quote ,body))
                  (,installer-var (make-installer ,def-var)))
             (lambda ()
               (let* ,(loop for probe-def in probes
                         collect (list (first probe-def)
                                       `(let ((,probe-val-var ,(second probe-def)))
                                          (if (functionp ,probe-val-var)
                                              (funcall ,probe-val-var ,def-var)
                                              ,probe-val-var))))
                 (declare (special ,@probe-vars))
                 (funcall ,installer-var)))))))

(defconstant +compass-product-code+ "{74013052-2EA3-4493-B92F-11A0368F9102}")

(define-condition db-shutdown-error (error) ()
  (:report "Unable to shutdown database."))

(defun db-running-p (db-file)
  (probe-file (make-pathname :type "lk" :defaults db-file)))

(defun host-path-namestring (path)
  (check-type path pathname)
  #+clisp (namestring path)
  #+sbcl (map 'string (lambda (c)
                        ;; Replace path separators
                        (if (eql c #\/)
                            #\\
                            c))
              (namestring path)))

(defeffect :shutdown-db ()
  (let ((dba-jar-pather (path-getter "DBAdmin.jar" :aux)))
    (lambda ()
      (declare (special *db-file* *progress-dir* *stopped-dbs*))
      (let* ((dba-class "com.mtg.makemeasandwich.DBAdmin")
             (args (list "127.0.0.1" "20931" "stopdbs" (host-path-namestring *db-file*)))
             (stopped-dbs ))
        (loop for running-p = (db-running-p *db-file*)
           while running-p
           do (with-simple-restart (retry-shutdown "Retry database shutdown")
                (let ((stopped-dbs (run-java dba-class
                                             :classpath (list (funcall dba-jar-pather)
                                                              (host-path-namestring
                                                               (merge-pathnames #P"java/progress.jar"
                                                                                *progress-dir*)))
                                             :defs `(("Install.Dir" ,(host-path-namestring *progress-dir*)))
                                             :args args)))
                  (unless (or (not (db-running-p *db-file*))
                              ;; If there were dbs, we'll wait for them
                              stopped-dbs)
                    (error 'db-shutdown-error))
                  ;; We're trusting that we'll never get a legit return from a
                  ;; failed shutdown (or else this will hang)
                  (loop while (db-running-p *db-file*)
                     do (sleep 1))

                  ;; Save the databases for later restart
                  (setf *stopped-dbs* stopped-dbs))))))))

(defun strip-dbpath (db-file)
  (check-type db-file (or string pathname))
  (let ((path (cl-fad:pathname-as-file db-file)))
    (make-pathname :type nil :defaults path)))

(defeffect :add-st (st-file)
  (check-type st-file (or string pathname))
  (let* ((st-path (cl-fad:pathname-as-file st-file))
         (pather (path-getter st-path :src)))
    (assert (not (cl-fad:directory-pathname-p st-path)) ()
            "~S is not a file pathname." st-path)
    (lambda ()
      (declare (special *db-file*))
      (let ((st-path (funcall pather))
            (prostrct-path (progress-bin #P"bin/_dbutil.exe")))
        (require-file prostrct-path "Progress binary" "can't run PROSTRCT")
        (require-file st-path "structure file")
        (require-file *db-file* "database file")
        ;; No good way to do error checking, we have to assume success
        (external-program:run (progress-bin #P"bin/_dbutil.exe")
                              (list "prostrct"
                                    "add"
                                    (strip-dbpath *db-file*)
                                    st-path))))))

(defun mk-temp-file (name)
  #+sbcl (cl-fad:with-output-to-temporary-file (str))
  #+ccl (pathname (ccl::temp-file-name name))
  #+clisp (let ((stream (posix:mkstemp name)))
            (prog1
                (pathname stream)
              (close stream))))

(defun mk-temp-dir (name)
  #+clisp (posix:mkdtemp name)
  #-clisp (error "MK-TEMP-DIR not implemented in this Lisp."))

(defun cwd ()
  #+clisp (ext:cd)
  #+sbcl (truename ".")
  #+ccl (ccl:current-directory)
  #-(or clisp sbcl ccl) (error "CWD not implemented in this Lisp."))

(defun (setf cwd) (dir)
  (let ((path (cl-fad:pathname-as-directory dir)))
    #+clisp (ext:cd path)
    #+sbcl (sb-posix:chdir path)
    #+ccl (setf (ccl:current-directory) path)
    #-(or sbcl clisp ccl) (error "SETF CWD not implemented in this Lisp.")))

(defmacro with-cwd ((dir) &body body)
  (let ((dir-var (gensym "DIR"))
        (saved-cwd-var (gensym "SAVED-CWD"))
        (saved-dpd-var (gensym "SAVED-DPD")))
    `(let ((,saved-dpd-var *default-pathname-defaults*)
           (,saved-cwd-var (cwd))
           (,dir-var (cl-fad:pathname-as-directory ,dir)))
       (format *debug-io* "Saved DPD is ~S~%" ,saved-dpd-var)
       (setf (cwd) ,dir
             *default-pathname-defaults* ,saved-dpd-var)
       (format *debug-io* "Restored saved DPD ~S (~S)~%" ,saved-dpd-var *default-pathname-defaults*)
       (unwind-protect (progn ,@body)
         (setf (cwd) ,saved-cwd-var
               *default-pathname-defaults* ,saved-dpd-var)))))

(define-condition df-application-error (abl-error)
  ((df-path :initarg :path
            :reader df-path)))

(defeffect :add-df (df-file)
  (check-type df-file (or string pathname))
  (let* ((df-path (cl-fad:pathname-as-file df-file))
         (df-pather (path-getter df-path :src))
         (load-pather (path-getter "applydf.r" :aux)))
    (assert (not (cl-fad:directory-pathname-p df-path)) ()
            "~S is not a file pathname." df-path)
    (lambda ()
      (declare (special *db-file*))
      (let ((df-path (funcall df-pather))
            (load-proc (funcall load-pather))
            (comm-file (mk-temp-file "comm"))
            (temp-dir (mk-temp-dir "df-application")))
        (require-file df-path "definition file")
        (require-file load-proc "system procedure")
        (require-file *db-file* "database file")
        (unwind-protect
             (with-cwd (temp-dir)
               (with-simple-restart (skip-file "Skip the application of this df file")
                 (handler-case
                     (run-abl (merge-pathnames load-proc)
                              comm-file
                              (list "-db" *db-file* "-1")
                              (merge-pathnames df-path)
                              (merge-pathnames comm-file))
                   (abl-error (c)
                     (error 'df-application-error
                            :path df-path
                            :errors (abl-error-errors c))))))
          (delete-file comm-file)
          (cl-fad:delete-directory-and-files temp-dir))))))

(defeffect :msg (msg)
  (lambda ()
    (format t msg)))

(defun prompt-until (msg pred &key (key #'identity))
  "Prompt for a value with MSG until PRED returns true. PRED is passed the line read from input."
  (flet ((prompt ()
           (format *query-io* "~A: " msg)
           (force-output *query-io*)
           (read-line *query-io*)))
    (loop for val = (prompt)
       until (funcall pred (funcall key val))
       finally (return val))))

(defun directory-with-files (prompt initial-dirs &rest files)
  "Return a directory that contains all of FILES, prompting for directories as needed. The directory returned is the first directory in INITIAL-DIRS that contains all of FILES, or the first prompted value that meets the condition."
  (flet ((has-files-p (dir)
           (let ((dir (cl-fad:pathname-as-directory dir)))
             (reduce (lambda (v f)
                       (let ((f (cl-fad:pathname-as-file f)))
                         (and v (probe-file (merge-pathnames f dir)))))
                     files
                     :initial-value t))))
    ;; Check the initial dirs first
    (loop for d in initial-dirs
       if (has-files-p d)
       do (return-from directory-with-files d))
    ;; If that fails, prompt for a value
    (cl-fad:pathname-as-directory (prompt-until prompt #'has-files-p))))

(definstaller :cmax-4.3-db
  (:probes (*src-dir* #P"src/")
           (*res-dir* #P"res/")
           (*compass-install-dir* (if (boundp '*compass-install-dir*)
                                      *compass-install-dir*
                                      (directory-with-files "Unable to find the CompassMax database. Please enter the directory where CompassMax is installed"
                                                            (list (product-install-dir +compass-product-code+))
                                                            #P"dbase/compass.db")))
           (*dest-dir* (merge-pathnames #P"code/" *compass-install-dir*))
           ;; NOTE: the Progress tools assume %DLC% points to the
           ;; Progress install, or that Progress is installed as
           ;; C:\Progress\OpenEdge\
           (*progress-dir* (directory-with-files "Unable to find Progress programs. Please enter the directory where Progress is installed"
                                                 (list (or (env-value "DLC")
                                                           "C:\\Progress\\OpenEdge\\")
                                                       ;; Look for
                                                       ;; files even
                                                       ;; if DLC is invalid
                                                       "C:\\Progress\\OpenEdge\\")
                                                 #P"bin/prowin32.exe"
                                                 #P"bin/_mprshut.exe"
                                                 #P"bin/_dbutil.exe"))
           (*version* "4.3.1")
           (*product* "CompassMax")
           (*db-file* (merge-pathnames #P"compass.db"
                                       (directory-with-files "Unable to locate CompassMax database. Please enter the directory where compass.db is located"
                                                             (list (merge-pathnames #P"dbase\\" *compass-install-dir*))
                                                             #P"compass.db"))))
  (:pre-effects :shutdown-db
                (:msg "Adding areas...")
                (:add-st "df/addarea.st")
                (:msg "Done.~%")
                (:msg "Updating definitions...")
                (:add-df "df/delta2.df")
                (:msg "Done.~%")))

(definstaller :cmax-4.3-code
  ;; Copy the whole src directory
  (:probes (*src-dir* #P"src/")
           (*compass-install-dir*
            (if (boundp '*compass-install-dir*)
                *compass-install-dir*
              (directory-with-files "Unable to find CompassMax code. Please enter the directory where CompassMax was installed"
                                    (list (product-install-dir +compass-product-code+))
                                    ;; Arbitrary file
                                    #P"code/client/gui/dlgAddActivityw.r")))
           (*dest-dir* *compass-install-dir*)
           (*res-dir* #P"res/"))
  (:tree (:+ #P"./")))

(defeffect :run-installer (name)
  (lambda ()
    (funcall (find-installer name))))

(definstaller :cmax-4.3
  ;; Bind this in the outer installer so the user is only prompted
  ;; once when running the sub-installers.
  (:probes (*compass-install-dir*
            (directory-with-files "Unable to find the CompassMax installation directory. Please enter the directory where Progress is installed"
                                  (list (product-install-dir +compass-product-code+))
                                  #P"dbase/compass.db"
                                  #P"code/client/gui/dlgAddActivityw.r")))
  (:pre-effects (:msg "Copying files...")
                (:run-installer :cmax-4.3-code)
                (:msg "Done.~%")
                (:run-installer :cmax-4.3-db)))

;; TODO: Add exception handling for this (print a logging error and bail)
(defun log-error (condition logfile)
  (with-open-file (log logfile :direction :output)
    (trivial-backtrace:print-backtrace condition :output log)))

(defun log-msg (msg logfile)
  (let ((timestamp (trivial-backtrace::date-time-string)))
    (with-open-file (log logfile :direction :output :if-exists :append)
      (format log "~A: ~A~%" timestamp msg))))

(defun run-compass-installer (name)
  "Run the CompassMax installer designated by NAME, returning T if the installer succeeded or NIL otherwise. Error conditions related"
  (check-type name installer-designator)
  (flet ((bail (logfile msg &rest args)
           (format t "~&********~%~A A log file is located at ~A. Please contact Maineline Technology Group (1-800-354-2525) for assistance.~%"
                   (apply #'format nil msg args)
                   logfile)
           (return-from run-compass-installer nil)))
    (let* ((installer (find-installer name))
           (log-dir (mk-temp-dir "compass-install"))
           (log-file (merge-pathnames #P"install.log" log-dir)))
      (handler-bind
          ((df-application-error
            (lambda (err)
              (log-error err log-file)
              (bail log-file "Oh no! There was an error while updating the database.")))
           ;; we run ABL scripts either to apply df files, or do
           ;; initialization
           ;; TODO: Does ABL-ERROR include the script that was run?
           (abl-error
            (lambda (err)
              (log-error err log-file)
              (bail log-file "There was an error initializing the database.")))
           (simple-file-error
            (lambda (err)
              (log-error err log-file)
              (bail log-file "There was an error installing new code.")))
           (warning
            (lambda (c)
              ;; Log it, but don't bail
              (log-msg (format nil "~A" c) log-file)
              (muffle-warning c)))
           ;; Trap /everything/
           (condition
            (lambda (err)
              (log-error err log-file)
              (bail log-file "Yikes! There was an unknown error during installation."))))
        (funcall installer))
      ;; If we completed successfully, remove the temp directory
      (cl-fad:delete-directory-and-files log-dir :if-does-not-exist :ignore)
      ;; TODO: print success (in installer, not here)
      )))
