(defpackage #:shecl-bootstrap
  (:use #:cl)
  (:export #:bootstrap))

(in-package #:shecl-bootstrap)

(defun bootstrap (load-path)
  (load (merge-pathnames #P"asdf.fas" load-path))
  (load (merge-pathnames #P"shecl.fasb" load-path)))
