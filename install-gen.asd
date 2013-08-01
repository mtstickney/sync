;;;; install-gen.asd

(asdf:defsystem #:install-gen
  :serial t
  :description "A basic installer generator."
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-secure-read #:cl-fad #:external-program #:cffi)
  :components ((:file "package")
               (:file "install-gen")))
