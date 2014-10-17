;;;; probuild.lisp

(in-package #:probuild)

(annot:enable-annot-syntax)

@eval-always
(defclass abl-system (asdf:system)
  ())

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

(defmethod initialize-instance :around ((obj abl-module) &rest rest &key (pathname nil) &allow-other-keys)
  (if pathname
      (apply #'call-next-method obj rest)
      (apply #'call-next-method obj :pathname "" rest)))

;; (defmethod initialize-instance :after ((obj abl-module) &key (pathname nil path-p) &allow-other-keys)
;;   (unless path-p
;;     (format *debug-io* "Setting~%")
;;     (setf (asdf:component-pathname obj) "")))

;; Allow the use of bare keyword class names in system defs
@eval-always
(setf (find-class 'asdf::procedure-file) (find-class 'procedure-file)
      (find-class 'asdf::window-file) (find-class 'window-file)
      (find-class 'asdf::class-file) (find-class 'class-file)
      (find-class 'asdf::database-file) (find-class 'database-file)
      (find-class 'asdf::abl-module) (find-class 'abl-module)
      (find-class 'asdf::abl-system) (find-class 'abl-system))

;; TODO: set up proper output transformations rather than just
;; disabling them (want to be able to do an out-of-source build)
(defmethod asdf:output-files ((op asdf:compile-op) (component abl-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (values (list (asdf:apply-output-translations output-file)) t)))

(defmethod asdf:output-files ((op asdf:compile-op) (component class-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (values (list (asdf:apply-output-translations output-file)) t)))

(defmethod asdf:output-files ((op asdf:compile-op) (component database-file))
  (let ((output-file (merge-pathnames (make-pathname :type "db")
                                      (asdf:component-pathname component))))
    (values (list (asdf:apply-output-translations output-file)) t)))

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

(defun set-output-dir (path)
  (check-type path pathname)
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   `(:output-translations (,(merge-pathnames #P"**/*.*")
                            ,(merge-pathnames (merge-pathnames #P"**/*.*" path)))
                          :ignore-inherited-configuration)))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-system))
  (cons (list 'asdf:prepare-op)
        (mapcar (lambda (c) (list 'asdf:compile-op c))
                (asdf:component-children component))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-file))
  '())

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-module))
  (cons (list 'asdf:prepare-op)
        (mapcar (lambda (c) (list 'asdf:compile-op c))
                (asdf:component-children component))))

(defun db-connection-info (logical-name &rest opts &key singleuser (pathname (concatenate 'string logical-name ".db")) host port username password (alias nil aliasp))
  (cond
    ;; Can have the :alias keyword and one argument
    ((and aliasp (third opts))
     (error "Cannot specify other options for an :ALIAS db."))
    (aliasp (list :alias logical-name alias))
    (t (list :db (format nil "~{~A~^ ~}"
                         (remove nil
                                 (list
                                  (format nil "-db ~S" pathname)
                                  (format nil "-ld ~S" logical-name)
                                  (and singleuser "-1")
                                  (and host (format nil "-H ~A" host))
                                  (and port (format nil "-S ~A" port))
                                  (and username (format nil "-U ~S" username))
                                  (and password (format nil "-P ~S" password)))))))))
