;;;; probuild.asd

(asdf:defsystem #:probuild
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:asdf #:cl-annot #:uiop #:cl-fad)
  :components ((:file "package")
               (:file "builder")
               (:file "probuild")))
