;;;; probuild.asd

(asdf:defsystem #:probuild
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:asdf #:cl-annot)
  :components ((:file "package")
               (:file "probuild")))
