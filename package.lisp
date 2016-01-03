;;;; package.lisp

(defpackage #:clomp
  (:use #:cl #:clomp.types)
  (:export #:com-interface
           #:define-interface
           #:com-object)
  (:export #:iunknown
           #:add-ref
           #:release
           #:query-interface)
  (:export #:iclassfactory
           #:create-instance
           #:lock-server
           #:class-factory
           #:register-factory
           #:unregister-factory))
