(in-package #:cl-persist)

;; TODO: There will be less type dispatching to do if we use a leaf
;; structs instead of node + edge node. If we do that it costs some
;; memory (there are more leafs than leaf-parents), and we have to
;; relax the type decl for node children (it becomes '(or node leaf))
;; rather than a static 'node or t. It boils down to whether you get
;; more benefit out of avoiding gf type dispatch or array element
;; optimization.

;; FIXME: ARRAY-NODE-SIZE name conflicts with reader from persistent-array class

(defstruct (array-node (:constructor mk-array-node)
                       (:copier nil))
  ;; Only writable because of %array-node-push
  (children #() :type vector)
  ;; Max size, if nodes aren't fully allocated
  (size 0 :type unsigned-byte :read-only t))

(defun copy-array-node (node)
  (check-type node array-node)
  (let ((new-children (alexandria:copy-array (array-node-children 'children))))
    (mk-array-node :children new-children :size (array-node-size node))))

(defun array-node-length (node)
  (check-type node array-node)
  (length (array-node-children node)))

;; All indexes for a node are valid, and since the indexes for nodes
;; are specified as a number of bits, the number of children a node
;; can have must be a power of two. When appending to a node, the
;; vector of children may need to be extended. A common method is to
;; resize a node from size n to 2n+1.
(defun %array-node-push (item node)
  (check-type node array-node)
  (flet ((next-size (n)
           (if (= n 0)
               1
               (* 2 n))))
    ;; If the node is already full, we signal an error:
    (when (>= (array-node-length node)
              (array-node-size node))
      (error "Node is full, cannot push item"))
    (let* ((children (array-node-children node))
           (allocated (array-total-size children)))
      ;; We only need a resize if the children vector is already full,
      (when (>= (length children) allocated)
        (setf (array-node-children node)
              (adjust-array children
                            ;; and we don't want to allocate any more
                            ;; than we need to.
                            (min (array-node-size node)
                                 (next-size allocated)))))
      ;; Once the node's been resized (if needed), we can push the
      ;; element onto the vector.
      (vector-push item (array-node-children node)))))

(defun %array-node-set (node index value)
  (check-type node array-node)
  (setf (elt (array-node-children node) index) value))

(defun make-node (size &rest items)
  (let* ((array (make-array size
                            :fill-pointer 0
                            :adjustable t))
         (node (mk-array-node :children array :size size)))
    (loop for i in items
       for j from 1
       if (> j size)
       ;; ELT will throw an error, but including the max size is nice
       do (error "Too many items for node to hold, need <=~S~%" size)
       do (%array-node-push i node))
    node))

(defun insert-height (n fanout)
  (check-type n (integer 1))
  (check-type fanout (integer 2))
  (assert (= (logcount fanout) 1) (fanout)
          "Fanout ~A is not a power of 2" fanout)
  (flet ((highest-empty-level (n fanout)
           ;; Note: assumes n > 1, other cases handled in COND
           ;; below. Note also that (1+ height) is safe, since we're
           ;; computing height from N and an insertion will never
           ;; increase the height of the tree by more than 1.
           (let ((max-height (1+ (ceiling (log n fanout)))))
             (loop for i from max-height downto 1
                if (= (mod n (expt fanout (1- i))) 0)
                return i))))
    (cond
      ;; Note: fanout must be (expt 2 bits) where (>= bits 1), so (= n
      ;; 2) will always be in the first node (there will be at least 2
      ;; entries for every node).
      ((or (= n 1) (= n 2)) 1)
      (t (highest-empty-level (1- n) fanout)))))

;; If and when nodes don't allocate all children on creation,
;; node-push needs to resize using f(0) = 1, f(x) = 2x.
;; This ensures the doubling behavior (not in the standard), and works
;; well with power-of-two sizes (a common default is 2x+1, which
;; wastes space for power-of-two sizes).
(defun node-push (item node)
  (check-type vector vector)
  (when (not (vector-push item (slot-value node 'children)))
    (error "Node vector is full, cannot push ~S." item)))

(defun build-node-path (size max-height &rest values)
  "Construct a node from VALUES and a string of parent nodes up to MAX-HEIGHT."
  (check-type size (integer 1))
  (let ((node (apply #'%build-array-node size t values)))
    (loop for i from 2 to max-height
       do (setf node (%build-node node)))
    node))

;; FIXME: Name conflicts with reader from persistent-array class
;; (defun array-node-size (node)
;;   (check-type node vector)
;;   (array-dimension node 0))

(defun array-node-item (node index)
  (check-type index fixnum)
  (elt node index))

(defun array-node-index (node index height)
  "Return the index into NODE for a key INDEX, assuming NODE is of height HEIGHT."
  (check-type node vector)
  (check-type index integer)
  (let ((size (array-node-size node)))
    (multiple-value-bind (bits extra) (floor (log size 2))
      (when (not (= 0 extra))
        (error "Size of node (~S) is not a power of two" size))
      (logand (ash index (* bits (1- height))))
      ))
  (logand (ash index (* (log (length)) (1- height)))))

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
