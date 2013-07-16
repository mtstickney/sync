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
  (children #() :type vector))

(defun copy-array-node (node)
  (check-type node array-node)
  (let ((new-children (alexandria:copy-array (array-node-children node))))
    (mk-array-node :children new-children)))

(defun array-node-length (node)
  (check-type node array-node)
  (length (array-node-children node)))

;; All indexes for a node are valid, and since the indexes for nodes
;; are specified as a number of bits, the number of children a node
;; can have must be a power of two. When appending to a node, the
;; vector of children may need to be extended. A common method is to
;; resize a node from size n to 2n+1.
(defun %array-node-push (item node max-size)
  (check-type node array-node)
  (flet ((next-size (n)
           (if (= n 0)
               1
               (* 2 n))))
    ;; If the node is already full, we signal an error:
    (when (>= (array-node-length node)
              max-size)
      (error "Node is full, cannot push item"))
    (let* ((children (array-node-children node))
           (allocated (array-total-size children)))
      ;; We only need a resize if the children vector is already full,
      (when (>= (length children) allocated)
        (setf (array-node-children node)
              (adjust-array children
                            ;; and we don't want to allocate any more
                            ;; than we need to.
                            (min max-size
                                 (next-size allocated)))))
      ;; Once the node's been resized (if needed), we can push the
      ;; element onto the vector.
      (vector-push item (array-node-children node)))))

(defun %array-node-set (node index value)
  (check-type node array-node)
  (setf (elt (array-node-children node) index) value))

(defun array-node-item (node index)
  (check-type index fixnum)
  (elt (array-node-children node) index))

(defun make-array-node (size &rest items)
  (let* ((array (make-array size
                            :fill-pointer 0
                            :adjustable t))
         (node (mk-array-node :children array)))
    (loop for i in items
       for j from 1
       if (> j size)
       ;; ELT will throw an error, but including the max size is nice
       do (error "Too many items for node to hold, need <=~S~%" size)
       do (%array-node-push i node size))
    node))

(defun array-node-update (node index item)
  (check-type node array-node)
  (check-type index integer)
  (let ((new-node (copy-array-node node)))
    (%array-node-set new-node index item)
    new-node))

;; (defun array-node-add (node max-size &rest items)
;;   (check-type node array-node)
;;   (let ((new-node (copy-array-node node)))
;;     (loop for i in items
;;        do (%array-node-push i new-node max-size)
;;        finally (return new-node))))

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
  (logand (1- (ash 1 num-bits))
          ;; Right-shifting the key by (1- height) groups of NODE-BITS
          ;; bits and ANDing with the mask yields our index:
          (ash key (- (* num-bits (1- height))))))

(defun %array-add (coll x)
  "Append X to the persistent-array COLL in-place."
  (check-type coll persistent-array)
  (let* ((node-size (array-node-size coll))
         (insert-height (insert-height (1+ (array-size coll))
                                       node-size)))
    (cond
      ;; If we need a new root node
      ((> insert-height (array-height coll))
       (setf (slot-value coll 'root)
             (make-array-node node-size
                              (array-root coll)
                              (make-parents node-size (array-height coll) x)))
       (incf (slot-value coll 'size))
       (incf (slot-value coll 'height)))
      ;; Otherwise find the rightmost node at insert-height and push a
      ;; chain of parents for the child.
      (t (let ((node (array-root coll))
               (child (make-parents node-size (1- insert-height) x)))
           (loop for i from (array-height coll)
              downto (1+ insert-height)
              do (setf node (array-node-item (1- (array-node-length node)))))
           (%array-node-push child node node-size)
           (incf (slot-value coll 'size)))))
    coll))

(defun array-add (coll x)
  "Append X to the persistent-array COLL."
  (check-type coll persistent-array)
  (let* ((node-size (array-node-size coll))
         (insert-height (insert-height (1+ (array-size coll))
                                       node-size)))
    (cond
      ((> insert-height (array-height coll))
       (let ((new-root (make-array-node node-size
                                        (array-root coll)
                                        (make-parents node-size (array-height coll) x))))
         (make-instance 'persistent-array
                        :root new-root
                        :node-bits (array-node-bits coll)
                        :size (1+ (array-size coll))
                        :height (1+ (array-height coll))
                        :tail (array-tail coll))))
      (t (let* ((node (copy-array-node (array-root coll)))
                (new-root node)
                (child (make-parents node-size (1- insert-height) x)))
           ;; Traverse the last children to the insertion level,
           ;; copying as we go.
           (loop for i from (array-height coll)
              downto (1+ insert-height)
              do (let ((last-idx (1- (array-node-length node))))
                   ;; Set node's last child to a copy of node's last child
                   (%array-node-set node last-idx
                                    (copy-array-node (array-node-item node last-idx)))
                   ;; Then move to the copied child
                   (setf node (array-node-item node last-idx))))
           (%array-node-push child node node-size)
           (make-instance 'persistent-array
                          :root new-root
                          :node-bits (array-node-bits coll)
                          :size (1+ (array-size coll))
                          :height (array-height coll)
                          :tail (array-tail coll)))))))

(defmethod add ((coll persistent-array) x &rest xs)
  ;; Add the first element non-destructively
  (let ((new-coll (array-add coll x)))
    ;; The rest can be added in-place, since only the edge copied by
    ;; the add above will be mutated.
    (loop for x in xs
       do (%array-add coll x))
    new-coll))

(defun array-update (coll key val)
  (check-type coll persistent-array)
  (check-type key integer)
  (assert (< key (array-size coll)) (key)
          "Index ~S is too large" key)
  (let ((node (copy-array-node (array-root coll)))
        (new-root node)
        (bits (array-node-bits coll)))
    (loop for h from (array-height coll) downto 2
       do (let ((idx (array-node-index bits key h)))
            ;; Set node's child to a copy of node's child
            (%array-node-set node idx
                             (copy-array-node (array-node-item node idx)))
            ;; Then move to the copied child
            (setf node (array-node-item node idx))))
    (%array-node-set node (array-node-index bits key 1)
                     val)
    (make-instance 'persistent-array
                   :root new-root
                   :node-bits (array-node-bits coll)
                   :size (array-size coll)
                   :height (array-height coll)
                   :tail (array-tail coll))))

(defmethod update ((coll persistent-array) key val &rest others)
  (let ((new-coll (array-update coll key val)))
    (loop for (key . rest) on other by #'cddr
       if (endp rest)
       do (error "UPDATE requires an even number of arguments.")
       do (setf new-coll (array-update coll key (car rest))))
    new-coll))

;; TODO: look for places we used 'INTEGER instead of 'UNSIGNED-BYTE
(defmethod lookup ((coll persistent-array) x &rest xs)
  (check-type x unsigned-byte)
  (check-type xs null)
  (let ((node (array-root coll))
        (bits (array-node-bits coll)))
    (loop for h from (array-height coll) downto 1
       do (setf node (array-node-item node (array-node-index bits h x))))
    node))
