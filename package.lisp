;;;; package.lisp

(defpackage #:cl-persist
  (:use #:cl)
  (:export #:persistent-array
           #:persistent-map
           #:lookup
           #:add
           #:seq
           #:equals))
