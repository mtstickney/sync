;;;; cl-persist.lisp

(in-package #:cl-persist)

;;; Public API for the collections
(defgeneric size (structure)
  (:documentation "Return the number of entries in STRUCTURE."))

(defgeneric update (collection x &rest xs)
  (:documentation "Return a new collection updated with items X and XS. The meaning of X and XS may vary from collection to collection."))

(defgeneric add (collection x &rest xs)
  (:documentation "Return a collection with an item described by X and XS added."))

(defgeneric lookup (collection x &rest xs)
  (:documentation "Return the element of COLLECTION described by X and XS."))

;;; Persistent Array container

(defclass persistent-array ()
  ((root :initarg :root
         :reader array-root)
   (tail :initarg :tail
         :reader array-tail)
   (size :initarg :size
         :reader array-size)
   (height :initarg :height
           :reader array-height)
   (node-bits :initarg :node-bits
              :reader array-node-bits)
   (node-size :reader array-node-size)
   (key-mask :reader array-key-mask))
  (:default-initargs
   :root nil ;;(make-array-node)
    :tail nil
    :size 0
    :height 0
    :node-bits 5))

;; TODO: trim the extra junk out of here (and the class def)
(defmethod initialize-instance :after
    ((arr persistent-array) &key node-bits &allow-other-keys)
  (check-type node-bits (integer 1))
  (setf (slot-value arr 'node-size) (expt 2 node-bits)
        (slot-value arr 'key-mask) (1- (ash 1 node-bits))))

(defmethod size ((array persistent-array))
  (array-size array))

(defun print-array-node (node stream)
  (print-object (array-node-children node) stream))

(defmethod print-object ((object persistent-array) stream)
  ;; TODO: replace with something that doesn't depend on TCO (or use
  ;; recursion without it)
  (labels ((print-items (node height)
             (if (= height 0)
                 (format stream "~S, " node)
                 (loop for i across (array-node-children node)
                    do (print-items i (1- height))))))
    (format stream "[")
    (when (>  (array-size object) 0)
      (print-items (array-root object) (array-height object)))
    (format stream "]")))
