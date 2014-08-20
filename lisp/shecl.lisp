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
  (setf *object-pools* (delete p *object-pools*))
  (values))

@export
(defun safe-eval-string (str)
  (declare (special *pool*))
  (let ((*debugger-hook* (lambda (c old-hook)
                           (declare (ignore old-hook))
                           (return-from safe-eval-string
                             (values c (write-to-string c :escape nil))))))
    (let* ((form (read-from-string str))
             (obj (eval form)))
        (if (boundp '*pool*)
            (add-to-pool obj *pool*)
            obj))))

@export
(defun safe-read-from-string (str)
  (declare (special *pool*))
  (let ((*debugger-hook* (lambda (c old-hook)
                           (declare (ignore old-hook))
                           (return-from safe-read-from-string
                             (values c (write-to-string c :escape nil))))))
    (let ((form (read-from-string str)))
        (if (boundp '*pool*)
            (add-to-pool form *pool*)
            form))))

@export
(defun safe-apply (func args)
  (declare (special *pool*))
  (let ((*debugger-hook* (lambda (c old-hook)
                           (declare (ignore old-hook))
                           (return-from safe-apply
                             (values c (write-to-string c :escape nil))))))
    (let ((val (apply func args)))
        (if (boundp '*pool*)
            (add-to-pool val *pool*)
            val))))
