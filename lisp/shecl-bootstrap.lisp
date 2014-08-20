(defpackage #:shecl-bootstrap
  (:use #:cl)
  (:export #:bootstrap))

(in-package #:shecl-bootstrap)

(defun bootstrap (load-path)
  (handler-case
      (let* ((load-pathname (parse-namestring load-path))
             (load-directory (make-pathname :defaults load-pathname
                                            :name nil :type nil)))
        (load (merge-pathnames #P"asdf.fas" load-path))
        (load (merge-pathnames #P"shecl.fasb" load-path)))
    (t (c) (values c (write-to-string c :escape nil)))))
