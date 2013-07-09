(in-package #:cl-persist)

;; Used for dispatching
(defstruct (array-node (:copier nil)
                       (:constructor %make-array-node))
  (children #() :type vector :read-only t))

(defun make-array-node (&key size leaf)
  "Return a new array node to hold SIZE elements, as an edge node if LEAF is true."
  (check-type size (integer 1))
  (make-array size
              :fill-pointer 0
              :element-type (if leaf t `(vector * ,size))))

(defun copy-array-node (node)
  (check-type node vector)
  (alexandria:copy-array node))

;; This ensures the doubling behavior (not in the standard), and works
;; well with +NODE-SIZE+ = 32
(defun node-push (item vector)
  (check-type vector vector)
  (when (not (vector-push item vector))
    (error "Node vector is full, cannot push ~S." item)))

;; Like update, but without having to create a copy of the initial
;; empty node
(defun %build-array-node (size leaf-p &rest items)
  (check-type size (integer 1))
  (let ((node (make-array-node :size size :leaf leaf-p)))
    (loop for i in items
       do (node-push i node))
    node))

(defun %build-node-path (size max-height &rest values)
  "Construct a node from VALUES and a string of parent nodes up to MAX-HEIGHT."
  (let ((node (apply #'%build-array-node size t values)))
    (loop for i from 2 to max-height
       do (setf node (%build-node node)))
    node))

(defun array-node-item (node index)
  (check-type index fixnum)
  (when (> index (length (array-node-children node)))
    (error "Index ~S out of bounds for array node, should be <~S"
           index
           (length (array-node-children node))))
  (elt (array-node-children node) index))

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
