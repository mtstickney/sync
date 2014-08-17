;;;; shecl.lisp

(defpackage #:shecl
  (:use #:cl #:cl-annot)
  (:export #:*pool*))

(in-package #:shecl)

(annot:enable-annot-syntax)

(defvar *object-pools* '())

(defstruct obj-pool
  (objs '() :type list))

@export
(defun make-pool ()
  (let ((p (make-obj-pool)))
    (push p *object-pools*)
    p))

(defun add-to-pool (obj p)
  (push obj (obj-pool-objs p))
  obj)

@export
(defun release-pool (p)
  (delete p *object-pools*)
  (values))

@export
(defun safe-eval-string (str)
  (declare (special *pool*))
  ;; Consider handling stuff by binding *debugger-hook*, which handles
  ;; the whole SIGNAL thing properly.
  (handler-case
      (let* ((form (read-from-string str))
             (obj (eval form)))
        (if (boundp '*pool*)
            (add-to-pool obj *pool*)
            obj))
    (serious-condition (c)
      (values c (write-to-string c :escape nil)))))

@export
(defun safe-read-from-string (str)
  (declare (special *pool*))
  (handler-case
      (let ((form (read-from-string str)))
        (if (boundp '*pool*)
            (add-to-pool form *pool*)
            form))
    (serious-condition (c)
      (values c (write-to-string c :escape nil)))))

@export
(defun safe-apply (func args)
  (declare (special *pool*))
  (handler-case
      (let ((val (apply func args)))
        (if (boundp '*pool*)
            (add-to-pool val *pool*)
            val))
    (serious-condition (c)
      (values c (write-to-string c :escape nil)))))
