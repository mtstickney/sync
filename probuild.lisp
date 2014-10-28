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
   (inherit-databases :initarg :inherit-databases :accessor inherit-databases)
   (builder :accessor builder :initform nil))
  (:default-initargs
   :databases '()
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

(defun changes-dbs-p (component)
  (and (typep component 'abl-module)
       ;; If it has a non-NIL db list or dis-inherits dbs, it changes
       ;; the db set.
       (or (and (slot-boundp component 'databases)
                (databases component))
           (null (inherit-databases component)))))

(defun db-module (component)
  (check-type component asdf:component)
  (loop with c = component
     while (and (asdf:component-parent c)
                (not (changes-dbs-p c)))
     do (setf c (asdf:component-parent c))
     finally (return c)))


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

;; Component building protocol
(defgeneric get-builder (component builder-class)
  (:method (component (builder-class symbol))
    (get-builder component (find-class builder-class))))

(defun component-builder-args (component)
  (let* ((dbs (component-databases 'asdf:compile-op component))
         (connect-args (apply #'append (mapcar (lambda (db-spec)
                                                 (cdr (apply #'db-connection-info db-spec)))
                                               dbs))))
    (append (progress-args (asdf:component-system component))
            connect-args)))

;; Exec-builder stuff
(defmethod get-builder ((component abl-file) (builder-class (eql (find-class 'exec-builder))))
  (make-instance builder-class :pro-args (component-builder-args component)))


;; Server-builder stuff
(defmethod get-builder ((component abl-file) (builder-class (eql (find-class 'server-builder))))
  (let* ((db-module (db-module component))
         (builder (or (builder db-module)
                      (make-instance 'server-builder
                                     :pro-args (component-builder-args component)))))
    ;; Remember, it returns the value
    (setf (builder db-module) builder)))

(defmethod asdf:perform ((op asdf:compile-op) (component abl-file))
  (let* ((builder-class (builder-class (asdf:component-system component)))
         (builder (get-builder component builder-class))
         (code-dir (asdf:component-pathname (asdf:component-system component)))
         (output-file (first (asdf:output-files 'asdf:compile-op component))))
    (build-file builder code-dir (asdf:component-pathname component)
                output-file
                :save-into (if (typep component 'class-file)
                               ;; COMPILE does funny things with
                               ;; SAVE-INTO for classes.
                               (output-directory)
                               (cl-fad:pathname-directory-pathname output-file)))))

;; After all the children have been compiled, shut down the builder if
;; necessary.
(defmethod asdf:perform :after ((op asdf:compile-op) (component abl-module))
  (let ((system (asdf:component-system component))
        (builder (builder component)))
    (when (and (eq (builder-class system) 'server-builder)
               builder)
      (shutdown-server builder)
      (setf (builder component) nil))))

(defun set-output-dir (path)
  (check-type path pathname)
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   `(:output-translations (,(merge-pathnames #P"**/*.*")
                            ,(merge-pathnames (merge-pathnames #P"**/*.*" path)))
                          :ignore-inherited-configuration
                          :disable-cache)))
