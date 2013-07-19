(in-package #:cl-persist)

(defun array-node-push (item node max-size)
  (check-type node vector)
  ;; Double the size with a special case for 0 instead of using 2n+1
  ;; (yields power-of-two sized arrays, which we want).
  (flet ((next-size (n)
           (if (= n 0)
               1
               (* 2 n))))
    ;; If the node is already full, we signal an error:
    (when (>= (length node)
              max-size)
      (error "Node is full, cannot push item"))
    (let* ((allocated (array-total-size node))
           ;; Re-allocate if necessary
           (new-node (if (>= (length node) allocated)
                         (adjust-array node (min max-size
                                                 (next-size allocated)))
                         node)))
      (vector-push item new-node)
      ;; Return the new array so it can be stored somewhere
      ;; (adjust-array may not happen in-place)
      new-node)))

(defun make-array-node (size &rest items)
  (let* ((array (make-array size
                            :fill-pointer 0
                            :adjustable t)))
    (loop for i in items
       for j from 1
       if (> j size)
       ;; ELT will throw an error, but including the max size is nice
       do (error "Too many items for node to hold, need <=~S~%" size)
       do (setf array (array-node-push i array size)))
    array))

;; Inserting nodes can be thought of as counting in base K for a K-way
;; tree. The ith digit ticks over every K^i numbers (so if we're
;; just counting the number N, the digits that have changed are all i
;; where N MOD K^i = 0). In our case, the largest changed digit shares
;; a parent and needs to be pushed onto a node, while the rest will be
;; freshly-allocated. We're interested in
(defun insert-height (n fanout)
  "Return the largest height in a FANOUT-way tree where a new node will
be inserted when inserting the Nth item. This is the height at which a
chain of newly-allocated parent nodes needs to be pushed onto an
existing node."
  (check-type n (integer 1))
  (check-type fanout (integer 2))
  (assert (= (logcount fanout) 1) (fanout)
          "Fanout ~A is not a power of 2" fanout)
  (flet ((highest-empty-level (n fanout)
           ;; An insertion will never increase the height of the tree
           ;; to more than the number of digits (base FANOUT) of N
           (let ((max-height (ceiling (log n fanout)))) ; = (1- (digits n fanout))
             (loop for i from max-height downto 0
                if (= (mod n (expt fanout i)) 0)
                return (1+ i)))))
    (cond
      ;; Note: fanout must be (expt 2 bits) where (>= bits 1), so (= n
      ;; 2) will always be in the first node (there will be at least 2
      ;; entries for every node).
      ((or (= n 1) (= n 2)) 1)
      (t (highest-empty-level (1- n) fanout)))))

(defun make-parents (size max-parents item)
  "Return a chain of up to MAX-PARENTS parents for ITEM."
  (let ((node item))
    (loop for i from 1 to max-parents
       do (setf node (make-array-node size node)))
    node))

;; To get the child of a node in the tree corresponding to a
;; particular key, we must first extract an index for this particular
;; level of the tree.
(defun array-node-index (node-bits height key)
  (check-type node-bits (integer 1))
  (check-type height (integer 1))
  (check-type key integer)
  ;; If NODE-BITS is the number of bits for a node index, the portion
  ;; of the key we want is the (1- height)th group of NODE-BITS bits,
  ;; from least significant to most. First we construct a mask of
  ;; NODE-BITS bits:
  (logand (1- (ash 1 node-bits))
          ;; Right-shifting the key by (1- height) groups of NODE-BITS
          ;; bits and ANDing with the mask yields our index:
          (ash key (- (* node-bits (1- height))))))

(defun copy-persistent-array (arr)
  ;; Shallow copy (intentional, nodes will be copied as part of traversal
  (make-instance 'persistent-array
                 :root (array-root arr)
                 :tail (array-tail arr)
                 :size (array-size arr)
                 :height (array-height arr)
                 :node-bits (array-node-bits arr)))

(defun array-add (coll x node-copier parray-copier)
  "Append X to the persistent-array COLL, copying nodes with NODE-COPIER and COLL with PARRAY-COPIER."
  (check-type coll persistent-array)
  (let* ((node-size (array-node-size coll))
         (insert-height (insert-height (1+ (array-size coll))
                                       node-size))
         (new-array (funcall parray-copier coll)))
    (cond
      ((> insert-height (array-height coll))
       (let ((new-root (if (null (array-root coll))
                           (make-array-node node-size
                                            (make-parents node-size (array-height coll) x))
                           (make-array-node node-size
                                            (array-root coll)
                                            (make-parents node-size (array-height coll) x)))))
         (setf (slot-value new-array 'height) (1+ (array-height coll))
               (slot-value new-array 'root) new-root)
         new-array))
      (t (let* ((node (funcall node-copier (array-root coll)))
                (new-root node)
                (child (make-parents node-size (1- insert-height) x)))
           ;; Traverse the last children to the insertion level,
           ;; copying as we go.
           (loop for i from (array-height coll)
              downto (1+ insert-height)
              do (let* ((last-idx (1- (length node)))
                        (last-child-copy (funcall node-copier
                                                  (elt node last-idx))))
                   ;; Set node's last child to a copy of node's last
                   ;; child
                   (setf (elt node last-idx) last-child-copy)
                   ;; Then move to the copied child
                   (setf node last-child-copy)))
           (array-node-push child node node-size)
           (setf (slot-value new-array 'root) new-root))))
    (setf (slot-value new-array 'size) (1+ (array-size coll)))
    new-array))

(defmethod add ((coll persistent-array) x &rest xs)
  ;; Add the first element non-destructively
  (let ((new-coll (array-add coll x #'alexandria:copy-array #'copy-persistent-array)))
    ;; The rest can be added in-place, since only the edge copied by
    ;; the add above will be mutated.
    (loop for x in xs
       do (array-add new-coll x #'identity #'identity))
    new-coll))

;; TODO: Could the traverse-while-copying thing be extracted into a
;; (multiple-value-bind (last-child new-root) (copy-traverse node
;; key)) thing, maybe with a :start-height and :end-height?

(defun array-update (bits root height key val)
  (check-type bits fixnum)
  (check-type root vector)
  (check-type height (integer 1))
  (check-type key unsigned-byte)

  ;; Note that we assume key is within bounds for the array (check is
  ;; done in UPDATE
  (let* ((node (alexandria:copy-array root))
         (new-root node))
    (loop for h from height downto 2
       do (let ((idx (array-node-index bits h key)))
            ;; Set node's child to a copy of node's child
            (setf (elt node idx) (alexandria:copy-array (elt node idx)))
            ;; Then move to the copied child
            (setf node (elt node idx))))
    (setf (elt node (array-node-index bits 1 key)) val)
    new-root))

(defmethod update ((coll persistent-array) key val &rest others)
  (flet ((update-root (root key val)
           (array-update (array-node-bits coll)
                         root
                         (array-height coll)
                         key
                         val)))
    (let ((new-root (update-root (array-root coll)  key val))
          (new-coll (copy-persistent-array coll)))
      (loop for (key . rest) on others by #'cddr
         if (endp rest)
         do (error "UPDATE requires an even number of arguments.")
         if (>= key (array-size coll))
         do (error "Index ~S too large" key)
         do (setf new-root (update-root new-root key (car rest))))
      (setf (slot-value new-coll 'root) new-root)
      new-coll)))

;; TODO: look for places we used 'INTEGER instead of 'UNSIGNED-BYTE
(defmethod lookup ((coll persistent-array) x &rest xs)
  (check-type x unsigned-byte)
  (check-type xs null)
  (let ((node (array-root coll))
        (bits (array-node-bits coll)))
    (loop for h from (array-height coll) downto 1
       do (setf node (elt node (array-node-index bits h x))))
    node))
