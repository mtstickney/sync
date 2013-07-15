(in-package #:cl-persist)

;; TODO: There will be less type dispatching to do if we use a leaf
;; structs instead of node + edge node. If we do that it costs some
;; memory (there are more leafs than leaf-parents), and we have to
;; relax the type decl for node children (it becomes '(or node leaf))
;; rather than a static 'node or t. It boils down to whether you get
;; more benefit out of avoiding gf type dispatch or array element
;; optimization.


;; TODO: benchmark a gf MAKE-INTERNAL-NODE/MAKE-EDGE-NODE (with a call
;; to ARRAY-NODE-SIZE vs. a closure returned from a gf
;; NODE-CONSTRUCTOR.

(defstruct (node (:conc-name nil)))

(defstruct (r-node (:include node)
                   (:copier nil))
  (children #() :type (vector t *) :read-only t))

(defstruct (internal-node (:include node)
                          (:constructor mk-internal-node)
                          (:copier nil))
  (children #() :type (vector node *) :read-only t))

(defstruct (edge-node (:include node)
                      (:constructor mk-edge-node)
                      (:copier nil))
  (children #() :type vector :read-only t))

(defstruct (leaf (:copier nil))
  (value nil :read-only t))

;; Option 1: two specialized methods to make different node types
(defmacro node-constructor (name class child-type node-constructor)
  `(defmethod ,name ((coll ,class) &rest items)
       (let* ((size (array-node-size coll))
              (array (make-array size
                                 :fill-pointer 0
                                 :element-type (quote ,child-type))))
         (loop for i in items
            for j from 1
            if (> j size)
            do (error "Too many items for node to hold, need <=~S" size)
            do (check-type i ,child-type)
              (vector-push i array))
         (,node-constructor :children array))))

(node-constructor make-internal-node persistent-array node mk-internal-node)
(node-constructor make-edge-node persistent-array t mk-edge-node)

(defmethod node-builder ((coll persistent-array) &key edge &allow-other-keys)
  (macrolet ((node-builder-body (ctor child-type)
               ;; SIZE and ITEMS are captured from the environment
               `(let ((array (make-array size
                                         :fill-pointer 0
                                         :element-type (quote ,child-type))))
                  (loop for i in items
                     for j from 1
                     if (> j size)
                     do (error "Too many items for node to hold, need <=~S~%" size)
                     do (check-type i ,child-type)
                       (vector-push i array))
                  (,ctor :children array))))
    (let ((size (array-node-size coll)))
      (if edge
          (lambda (&rest items)
            (node-builder-body mk-edge-node t))
          (lambda (&rest items)
            (node-builder-body mk-internal-node node))))))

(defmethod make-me-a-node ((coll persistent-array) &rest items)
  (let* ((size (array-node-size coll))
         (array (make-array size
                               :fill-pointer 0
                               :element-type 'r-node)))
    (loop for i in items
       for j from 1
       if (> j size)
       do (error "Too many items for node to hold, need <=~S~%" size)
       do (vector-push i array))
    (make-r-node :children array)))

;; Best time 0.033 seconds, average ~0.041
;; Best time 0.081 seconds, average ~0.093
(defun benchmark-gf-builders ()
  (let ((arr (make-instance 'persistent-array :node-bits 5))
        (ibuilder #'make-internal-node)
        (ebuilder #'make-edge-node)
        (internal-child (mk-edge-node))
        (edge-child 4)
        (node))
    (time (progn
            (dotimes (i 10000)
              (setf node (funcall ibuilder arr internal-child internal-child internal-child)))
            (dotimes (i 10000)
              (setf node (funcall ebuilder arr edge-child edge-child edge-child)))))))

;; Best time 0.031 seconds, average ~0.42
;; Best time 0.080 seconds, average ~0.90
(defun benchmark-gf-closure ()
  (let* ((arr (make-instance 'persistent-array :node-bits 5))
         (ibuilder (node-builder arr))
         (ebuilder (node-builder arr :edge t))
         (internal-child (mk-edge-node))
         (edge-child 4)
         (node))
    (time (progn
            (dotimes (i 10000)
              (setf node (funcall ibuilder internal-child internal-child internal-child)))
            (dotimes (i 10000)
              (setf node (funcall ebuilder edge-child edge-child edge-child)))))))

(defun benchmark-gf-rnode ()
  (let* ((arr (make-instance 'persistent-array :node-bits 5))
         (ibuilder #'make-internal-node)
         (ebuilder #'make-edge-node)
         (internal-child (make-r-node))
         (edge-child (make-leaf :value 4))
         (node))
    (time (progn
            (dotimes (i 10000)
              (setf node (funcall ibuilder arr internal-child internal-child internal-child)))
            (dotimes (i 10000)
              (setf node (funcall ebuilder arr edge-child edge-child edge-child)))))))

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

(defun copy-array-node (node)
  (check-type node node)
  (alexandria:copy-array (slot-value node 'children)))

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
