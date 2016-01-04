;;;; package.lisp

(defpackage #:clomp
  (:use #:cl #:clomp.types)
  (:export #:com-interface
           #:define-interface
           #:com-object
           #:class-id)
  (:export #:iunknown
           #:add-ref
           #:release
           #:query-interface)
  (:export #:iclassfactory
           #:client-class-id
           #:create-instance
           #:lock-server
           #:class-factory
           #:register-factory
           #:unregister-factory))
