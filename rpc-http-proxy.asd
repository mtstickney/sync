;;;; rpc-http-proxy.asd

(defpackage #:rpc-http-proxy-config
  (:use #:cl)
  (:export #:*config-dir*))

(defparameter rpc-http-proxy-config:*config-dir* #.(or *load-truename* *compile-file-truename*))

(asdf:defsystem #:rpc-http-proxy
  :description "Describe rpc-http-proxy here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-mtgnet
               #:cl-mtgnet-async
               #:clack
               #:clack-handler-wookie
               #:mtgnet-encryption
               #:uiop
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "rpc-http-proxy")))
