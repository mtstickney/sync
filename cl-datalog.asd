;;;; cl-datalog.asd

(asdf:defsystem #:cl-datalog
  :serial t
  :description "Describe cl-datalog here"
  :author "Matthew Stickney"
  :license "Specify license here"
  :depends-on (#:map-set)
  :components ((:file "package")
               (:file "cl-datalog")))
