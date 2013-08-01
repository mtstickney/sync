;;;; install-gen.lisp

(in-package #:install-gen)

;;; "install-gen" goes here. Hacks and glory await!

(defvar *defs* (make-hash-table :test 'equal))
(defvar *effects* (make-hash-table))

(define-condition missing-file ()
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

(define-condition abl-error ()
  ((errors :initarg :errors
           :reader abl-error-errors))
  (:documentation "Condition signalled to report errors from a callout to ABL.")
  (:report (lambda (condition stream)
             (format stream "~@<ABL returned with errors:~@:_~I~{~<~A: ~:_~I~W~@:_~:>~}~:>"
                     (abl-error-errors condition)))))

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
                         (merge-pathnames path ,base-symb))))
             ;; TODO: PROBE-FILE :src and :aux (but then we can't
             ;; build the code without having all the resources...)
             (ecase type
               (:src (fetcher *src-dir*))
               (:dest (fetcher *dest-dir*))
               (:aux (fetcher *res-dir*))
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

(defun tree-diff-applicator (cmd path)
  (check-type cmd keyword)
  (check-type path (or pathname string))
  (let ((src-pather (path-getter path :src))
        (dest-pather (path-getter path :dest)))
    (ecase cmd
      (:+ (lambda () (cl-fad:copy-file (funcall src-pather)
                                       (funcall dest-pather)
                                       :overwrite t)))
      (:- (lambda () (remove-file (funcall dest-pather)))))))

(defun tree-application (tree-spec)
  (check-type tree-spec list)
  (let ((applicators (mapcar (lambda (file-spec)
                               (apply #'tree-diff-applicator file-spec))
                             tree-spec)))
    ;; TODO: Add a top-level tree application restart
    (lambda ()
      (loop for app in applicators
         do (funcall app)))))

(defun st-file-applicator (file db-name)
  (check-type file (or pathname string))
  (check-type file (or pathname string))
  (let ((st-pather (path-getter file :aux)))
    (lambda ()
      (declare (special *progress-dir* *dest-dir*))
      (let* ((prostrct-path (merge-pathnames "bin/_dbutil.exe" *progress-dir*))
             (st-path (funcall st-pather))
             (db-path (merge-pathnames (cl-fad:pathname-as-file db-name)
                                       *dest-dir*))
             (db-file-path (merge-pathnames db-path (make-pathname :type "db"))))
        (require-file prostrct-path "Progress binary" "can't run PROSTRCT")
        (require-file st-path "structure file")
        (require-file db-file-path "database file")
        ;; Just have to assume this succeeds, because Progress doesn't
        ;; understand return codes.
        (external-program:run prostrct-path (list "prostrct" "add"
                                                  db-path
                                                  st-path))))))

(defun df-file-applicator (file db-file)
  (check-type file (or pathname string))
  (check-type db-file (or pathname string))
  (let ((pather (path-getter file :aux))
        (load-pather (path-getter "sys/load_df.r" :aux))
        (db-pather (path-getter db-file :dest)))
    (lambda ()
      (declare (special *dest-dir*))
      (let ((df-path (funcall pather))
            (db-file (funcall db-pather))
            (load-proc (funcall load-pather)))
        (require-file df-path "definition file")
        (require-file load-proc "system procedure")
        (require-file db-file "database file")
        (run-abl load-proc db-file df-path)))))

;; TODO: Should the sys/ dir have its own :sys type?
(defun data-file-loader (file db-file)
  (check-type file (or pathname string))
  (check-type db-file (or pathname string))
  (let ((pather (path-getter file :aux))
        (load-pather (path-getter "sys/load_d.r" :aux))
        (db-pather (path-getter db-file :dest)))
    (lambda ()
      (declare (special *dest-dir*))
      (let ((d-path (funcall pather))
            (db-file (funcall db-pather))
            (load-proc (funcall load-pather)))
        (require-file load-proc "system procedure")
        (require-file db-file "database file")
        (require-file d-path "data file")
        (run-abl load-proc db-file d-path)))))

;; TODO: make sure to handle EOF errors
(defun run-abl (proc system-args &rest args)
  (declare (special *progress-dir*))
  (check-type *progress-dir* (or string pathname))
  (labels ((prowin-args (proc system-args args)
             (concatenate 'list
                          (list "-p" proc
                                "-param" (format nil "~{~A~^,~}" args))
                          system-args)))
    (let* ((prowin-path (let ((path (merge-pathnames "bin/prowin32.exe"
                                                     *progress-dir*)))
                          (require-file path "Progress binary" "unable to run ABL procedure")
                          path))
           (result-str (with-output-to-string (fh)
                         (external-program:run
                          prowin-path
                          (prowin-args proc system-args args)))))
      ;; TODO: Use CL-SECURE-READ for this
      (let ((result (read-from-string result-str)))
        ;; TODO: c'mon now, can't recover from this
        (check-type result list)
        ;; TODO: Neither is this.
        (ecase (car result)
          (:data (apply #'values (cdr result)))
          (:error (error 'abl-error :errors (cdr result))))))))

(defmacro defeffect (name args &body body)
  (check-type args list)
  (check-type name symbol)
  `(setf (gethash (symbol-name ,name) *effects*)
         (lambda ,args ,@body)))

(defeffect :db-structure (db-file &rest st-files)
  (let ((applicators (map nil (lambda (f)
                                (st-file-applicator f db-file))
                          st-files)))
    (lambda ()
      (map nil #'funcall applicators))))

(defeffect :db-defs (db-file &rest df-files)
  (let ((applicators (map nil (lambda (f)
                                (df-file-applicator f db-file))
                          df-files)))
    (lambda ()
      (map nil #'funcall applicators))))

(defeffect :db-binary-data (db-file &rest d-files)
  (let ((applicators (map nil (lambda (f)
                                (data-file-loader f db-file))
                          d-files)))
    (lambda ()
      (map nil #'funcall applicators))))

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

(cffi:defcfun (reg-open-key-ex "RegOpenKeyExA" :library advapi32) :long
  (key :pointer)
  (subkey :pointer)
  (options :ulong)
  (regsam :ulong)
  (result :pointer))

(cffi:defcfun (reg-query-value-ex "RegQueryValueExA" :library advapi32) :long
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

(define-condition foreign-system-error ()
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
  (:method ((type (eql (cffi:foreign-enum-value 'registry-value-type :sz))) data size)
    (cffi:foreign-string-to-lisp data :max-chars size)))

(defun reg-read-val (base-key subkey value-name)
  (cffi:with-foreign-strings ((subkey-str subkey)
                              (value-str value-name))
    (cffi:with-foreign-objects ((key :pointer)
                                (type 'registry-value-type)
                                (data-size :ulong))
      (let (ret)
        (setf ret (reg-open-key-ex hkey-local-machine
                                   subkey-str
                                   0
                                   (cffi:foreign-enum-value 'reg-key-access :query-value)
                                   key))
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
                  code
                  (format nil "{~A}" code))))
    (format nil "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\~A" code)))

(defun product-installed-p (product-code)
  "Returns T if the product with the GUID PRODUCT-CODE is installed on the machine, NIL otherwise (this is true if the program appears in the 'Add/Remove Programs' listing."
  (let (subkey (product-subkey product-code))
    (reg-key-exists-p hkey-local-machine subkey)))

(defun product-install-dir (product-code)
  (let ((subkey (product-subkey product-code)))
    (reg-value-exists-p hkey-local-machine subkey "InstallLocation")))

;; TODO: Later
;; (defeffect :set-reg-key (key val)
;;   (etypecase key
;;     (string )
;;     )
;;   )

;; (defeffect :backup-if-space ()
;; )

(defun effects-applicator (effects)
  (let ((applicators (loop for e in effects
                        collect (apply (find-effect (car e))
                                       (cdr e)))))
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
      ;; TODO: bind special vars (*dest-dir* *src-dir*
      ;; *progress-dir*), probably with probes facility
      (when pre-applicator
        (funcall pre-applicator))
      (when tree-applicator
        (funcall tree-applicator))
      (when post-applicator
        (funcall post-applicator)))))

(defun find-installer (installer-designator)
  (check-type installer-designator (or symbol string installer))
  (etypecase installer-designator
    (installer installer-designator)
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
         (def-var (gensym "DEF")))
    `(setf (gethash ,(symbol-name name) *defs*)
           (let ((,def-var (quote ,body)))
             (lambda ()
               (let* ,(loop for probe-def in probes
                         collect (list (first probe-def)
                                       `(funcall ,(second probe-def) ,def-var)))
                 (declare (special ,@probe-vars))
                 (funcall (make-installer ,def-var))))))))

(definstaller cmax-5
    (:version "5.0.1.1")
  (:product "CompassMax")
  (:src-dir "res/")
  (:probes (*src-dir* (lambda (def)
                        (cdr (assoc :src-dir def))))
           (*dest-dir* #'find-dest-dir)
           (*progress-dir* #'find-progress-dir)
           (*version* (lambda (def)
                        (second (assoc :version def))))
           (*product* (lambda (def)
                        (second (assoc :product def)))))
  ;(:check-sources t) ; defaults to t, used to check resource paths
  ;when building
  (:tree (:+ "client/include/blargl.r")
         (:- "client/procedure/NotAThing.r"))
  (:pre-effects :backup-if-space)
  (:post-effects (:db-structure "dbase/addarea.st")
                 (:db-defs "df/mailingevent.df"
                           "df/area2.df"
                           "df/thingy.df")
                 (:db-binary-data "defaults.d")
                 (:db-text-data "table-data.txt") ; What's the
                                        ; extension again?
                 ;; Maybe don't want this
                 (:set-reg-key "HCOM\\Foo\\MS\\Blurb\\MTG\\Compass\\Version" "5.0.1.1")
                 ;; Tree copy with a special destination. Hmmm...
                 (:add-or-update-shortcut (merge-pathnames "CompassMax 4.3.2.6.lnk" (desktop-dir)))))
