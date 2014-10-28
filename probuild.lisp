;;;; probuild.lisp

(in-package #:probuild)

(annot:enable-annot-syntax)

(defclass abl-file (asdf:source-file)
  ())

@eval-always
(defclass procedure-file (abl-file)
  ((type :initform "p")))

@eval-always
(defclass window-file (abl-file)
  ((type :initform "w")))

@eval-always
(defclass class-file (abl-file)
  ((type :initform "cls")))

;; .st files
@eval-always
(defclass database-file (asdf:source-file)
  ((block-size :initarg :block-size :accessor block-size)
   (schema :initarg :schema :accessor db-schema)
   (data :initarg :data :accessor db-data))
  (:default-initargs
   :block-size 4 ;; 4kb block size, default in existing compass
    ;; install
    :schema nil
    :data nil))

@eval-always
(defclass abl-module (asdf:module)
  ((databases :initarg :databases :accessor databases)
   (inherit-databases :initarg :inherit-databases :accessor inherit-databases))
  (:default-initargs
   :databases nil
    :inherit-databases t))

@eval-always
(defclass abl-system (asdf:system abl-module)
  ((progress-args :initarg :progress-args :accessor progress-args)
   (builder-class :initarg :builder-class :accessor builder-class)
   ;; Note: ASDF bypasses initialize-instance (it uses some
   ;; change-class magic), so :default-initargs won't work here.
   (databases :initarg :databases :accessor databases :initform '())
   (inherit-databases :initarg :inherit-databases :accessor inherit-databases :initform t)))

;; Allow the use of bare keyword class names in system defs
@eval-always
(setf (find-class 'asdf::procedure-file) (find-class 'procedure-file)
      (find-class 'asdf::window-file) (find-class 'window-file)
      (find-class 'asdf::class-file) (find-class 'class-file)
      (find-class 'asdf::database-file) (find-class 'database-file)
      (find-class 'asdf::abl-module) (find-class 'abl-module)
      (find-class 'asdf::abl-system) (find-class 'abl-system))

(defmethod asdf:output-files ((op asdf:compile-op) (component abl-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defmethod asdf:output-files ((op asdf:compile-op) (component class-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defmethod asdf:output-files ((op asdf:compile-op) (component database-file))
  (let ((output-file (merge-pathnames (make-pathname :type "db")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defgeneric component-databases (op component)
  (:documentation "Return a list of database specifications in effect when OP is applied to COMPONENT."))

(defmethod component-databases (op (component (eql nil)))
  nil)

(defmethod component-databases (op component)
  ;; If the component doesn't potentially introduce a database, just
  ;; use the parent's list
  (let ((parent (asdf:component-parent component)))
    (component-databases op parent)))

(defmethod component-databases (op (component abl-module))
  (let ((local-dbs (databases component))
        (parent (asdf:component-parent component)))
    (if (not (inherit-databases component))
        local-dbs
        (append (component-databases op parent) local-dbs))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-module))
  (cons (list 'asdf:prepare-op)
        (mapcar (lambda (c) (list 'asdf:compile-op c))
                (asdf:component-children component))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-file))
  '())


(defun db-connection-info (logical-name &rest opts &key singleuser (pathname (concatenate 'string logical-name ".db")) host port username password (alias nil aliasp))
  (cond
    ;; Can have the :alias keyword and one argument
    ((and aliasp (third opts))
     (error "Cannot specify other options for an :ALIAS db."))
    (aliasp (list :alias logical-name alias))
    (t (cons :db (append
                  (list "-db" (format nil "~A" pathname))
                  (list "-ld" (format nil "~A" logical-name))
                  (and singleuser (list "-1"))
                  (and host (list "-H" (format nil "~A" host)))
                  (and port (list "-S" (format nil "~A" port)))
                  (and username (list "-U" (format nil "~A" username)))
                  (and password (list "-P" (format nil "~A" password))))))))

(defun output-directory ()
  (asdf:apply-output-translations #P"."))

(defmethod asdf:perform ((op asdf:compile-op) (component abl-file))
  (let* ((dbs (component-databases op component))
         (connect-args (apply #'append (mapcar (lambda (db-spec)
                                                 (cdr (apply #'db-connection-info db-spec)))
                                               dbs)))
         (*prowin-args* (append '("-b")
                                (progress-args (asdf:component-system component))
                                (if *progress-ini*
                                    (list "-basekey" "INI" "-ininame"
                                          (namestring *progress-ini*))
                                    '())
                               connect-args))
         (code-dir (asdf:component-pathname (asdf:component-system component)))
         (output-file (first (asdf:output-files op component))))
    (build-file code-dir
                (asdf:component-pathname component)
                output-file
                :save-into (if (typep component 'class-file)
                               (output-directory)
                               (cl-fad:pathname-directory-pathname output-file)))))

(defun set-output-dir (path)
  (check-type path pathname)
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   `(:output-translations (,(merge-pathnames #P"**/*.*")
                            ,(merge-pathnames (merge-pathnames #P"**/*.*" path)))
                          :ignore-inherited-configuration
                          :disable-cache)))
