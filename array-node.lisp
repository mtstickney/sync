(in-package #:cl-persist)

(defconstant +node-bits+ 5)
(defconstant +node-length+ (expt 2 +node-bits+))

(declaim (type fixnum +node-length+))
(declaim (type fixnum +node-bits+))

(defstruct (array-node (:copier nil))
  (children (make-array +node-length+ :fill-pointer 0 :element-type 'array-node))
  (height 1))

;; This ensures the doubling behavior (not in the standard), and works
;; well with +NODE-SIZE+ = 32
(defun node-vector-push (item vector)
  (check-type vector vector)
  (when (>= (fill-pointer vector) +node-length+)
    (error "Node vector is full, cannot push ~S." item))
  (let ((capacity (array-total-size vector)))
    (when (= (fill-pointer vector) capacity)
      (setf vector (adjust-array vector (if (= capacity 0) 1 (* 2 capacity)))))
    (vector-push item vector)))

(defun copy-array-node (node)
  (check-type node array-node)
  (make-array-node :children (alexandria:copy-array (array-node-children node))
                   :height (array-node-height node)))

;; Like update, but without having to create a copy of the initial
;; empty node
(defun %build-node (height &rest indices)
  (let* ((node (make-array-node :height height))
         (children (array-node-children node)))
    (loop for value in indices
       do (node-vector-push value children))
    node))

(defun %build-node-path (max-height &rest values)
  (let ((node (apply #'%build-node 1 values)))
    (loop for i from 2 to max-height
       do (setf node (%build-node node)))
    node))

(defgeneric node-item (node index)
  (:method ((node array-node) index)
    (check-type index fixnum)
    (if (not (has-index-p node index))
        (values nil nil)
        (values (elt (array-node-children node) index)
                t))))

(defun array-node-index (node index)
  (check-type node array-node)
  (check-type index integer)
  (let ((height (array-node-height node)))
    (logand (the fixnum (ash index (* +node-bits+ (1- height))))
            (the fixnum #x1F))))

(defun add-to-tree (root x)
  (check-type root array-node)
  (let* ((height (array-node-height root))
         (num-children (length (array-node-children root)))
         (last-child (elt root (1- num-children))))
    (cond
      ((= height 1)
       (add root x))
      ((= (length (array-node-children last-child)) +node-size+)
       ;; Last child is full, need a new one
       (add root (%build-array-path (1- height) x)))
      (t (update root
                 (1- num-children) (add-to-tree last-child x))))))

(defun array-tree-add (root val size)

  )

(defmethod add ((node array-node) value &rest values)
  (unless (endp values)
    (error "Too many arguments supplied to ADD for object of type ARRAY-NODE (expects 1)"))
  (let ((new-node (copy-array-node node)))
    (node-vector-push value (array-node-children new-node))
    new-node))

(defmethod update ((coll array-node) index &rest indexes)
  (let ((children (array-node-children coll))
        (new-node (copy-array-node)))
    (loop for (index . (val . rest))
       on (cons index indexes)
       by #'cddr
       do (when (> index (fill-pointer children))
            (error "Index ~S is out of bounds for array node, should be <=~S"
                   index
                   (fill-pointer children)))
         (setf (elt children index) val))))
