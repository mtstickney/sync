;;;; wappsy.asd

(asdf:defsystem #:wappsy
  :description "Describe wappsy here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:dynamo)
  :serial t
  :components ((:file "package")
               (:file "wappsy")))

