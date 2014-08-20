(defpackage #:shecl-bootstrap
  (:use #:cl)
  (:export #:bootstrap))

(in-package #:shecl-bootstrap)

(defun bootstrap ()
  (load (merge-pathnames #P"asdf.fas" *load-truename*))
  (load (merge-pathnames #P"shecl.fasb" *load-truename*)))
