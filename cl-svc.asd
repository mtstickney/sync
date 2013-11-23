;;;; cl-svc.asd

(asdf:defsystem #:cl-svc
  :serial t
  :description "Describe cl-svc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-svc")))

