;;;; cl-persist.asd

(asdf:defsystem #:cl-persist
  :serial t
  :description "Describe cl-persist here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "cl-persist")
               (:file "util")
               (:file "array-node")
               (:file "map-node")))
