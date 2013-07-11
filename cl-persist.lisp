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

;; Internal-use methods of persistent collections
(defgeneric node-constructor (collection)
  (:documentation "Return a function that can be used to construct a new node for COLLECTION, optionally containing some items. Lambda-list for the returned function is (&rest items). Specific collections may use keywords to specify constructors for different types of nodes."))

(defgeneric copy-node (node)
  (:documentation "Return a copy of NODE."))

;;; Persistent Array container

(defclass persistent-array ()
  ((root :initarg :root
         :reader array-root)
   (tail :initarg :tail
         :reader array-tail)
   (size :initarg :size
         :reader array-size)
   (node-bits :initarg :node-bits
              :reader array-node-bits)
   (node-size :reader array-node-size)
   (key-mask :reader array-key-mask))
  (:default-initargs
   :root nil ;;(make-array-node)
    :tail nil
    :size 0
    :node-bits 5))

(defmethod initialize-instance :after
    ((arr persistent-array) &key node-bits &allow-other-keys)
  (check-type node-bits (integer 1))
  (setf (slot-value arr 'node-size) (expt 2 node-bits)
        (slot-value arr 'key-mask) (1- (ash 1 node-bits))))

(defstruct leaf
  (value))

(defun update-tree (root index value)
  (check-type root array-node)
  (check-type index (integer 0))
  (let* ((height (array-node-height root))
         (node-index (array-node-index node index)))
    (if (= (array-node-height root) 1)
        (update root node-index value)
        (update root node-index (update-tree (node-item root node-index)
                                             index
                                             value)))))

;; TODO: replace elt with aref where necessary, add range checks (aref/elt
;; may or may not error on invalid access)
;; For now, vector must be continguous: (insert [a b c] 20 3) will
;; blow up
(defmethod update ((array persistent-array) index &rest values)
  (check-type index integer)
  (when (evenp (length values))
    (error "Object of type PERSISTENT-ARRAY requires an even number of arguments for UPDATE."))
  (loop for (index . (value . rest))
     on (cons index values)
     by #'cddr
     with size = (array-size array)
     with node = (array-root array)
     do (when (>= index size)
          (error "Index ~S is out of bounds, should be <~S" index size))
       (setf node (update-tree node index value))
     finally (return-from update node)))

(defmethod add ((array persistent-array) x &rest xs)
  (unless (endp xs)
    (error "Too many arguments supplied to ADD for object of type PERSISTENT-ARRAY (expects 1)"))
  (let ((size (array-size array))
        (root (array-root array))
        (tail (array-tail array)))
    (cond
      ((< size +node-length+)
       ;; Still in the first node, don't have a tail yet
       (make-instance 'persistent-array
                      :size (1+ size)
                      :root (add root x)))
      ((= size +node-length+)
       ;; first root is full, need a tail
       (make-instance 'persistent-array
                      :size (1+ size)
                      :root root
                      :tail (%build-array-node 1 x)))
      ((= (length tail) +node-length+)
       ;; Integrate tail and generate a new one
       (let* ((height (array-node-height root))
              (new-root (%build-array-node (1+ height)
                                          root
                                          (%build-array-path height x))))
         (make-instance 'persistent-array
                        :size (1+ size)
                        :root new-root
                        :tail (make-array-node))))
      (t (make-instance 'persistent-array
                        :size (1+ size)
                        :root root
                        :tail (add tail x))))))

(defmethod lookup ((array persistent-array) x &rest xs)
  (unless (endp xs)
    (error "Too many arguments supplied to LOOKUP for object of type PERSISTENT-ARRAY (expects 1)"))
  (let* ((node (array-root array))
         (height (array-node-height node)))
    (when (>= x (array-size array))
      (error "Index ~S out of bounds for array, should be < ~S"
             x
             (array-size array)))
    (cond
      ;; In the tail
      ((typep x `(integer ,(- (array-size array) +node-le))))
      )
    ;; Last one will set node to the stored item
    (loop for i from 1 to height
       do (setf node (array-node-item node (array-node-index node x))))
    node))

(defmethod size ((array persistent-array))
  (array-size array))

(defun print-array-node (node stream)
  (print-object (array-node-children node) stream))
