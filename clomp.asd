;;;; clomp.asd

(asdf:defsystem #:clomp
  :description "Describe clomp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:trivial-garbage
               #:cffi
               #:uuid
               #:babel
               #:trivial-features)
  :serial t
  :components ((:file "types")
               (:file "ffi")
               (:file "package")
               (:file "clomp")
               (:file "dispatch")))
