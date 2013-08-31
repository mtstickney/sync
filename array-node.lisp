(in-package #:cl-persist)

;;; TODO: investigate whether we really need the (null root) check
;;; when inserting, since VISIT will do a nil -> node conversion

;; TODO: Maybe start with non-fully-allocated nodes?
(defun make-array-node (size &rest items)
  (let* ((array (make-array size
                            :fill-pointer 0
                            :adjustable t)))
    (loop for i in items
       for j from 1
       if (> j size)
       ;; ELT will throw an error, but including the max size is nice
       do (error "Too many items for node to hold, need <=~S~%" size)
       do (setf array (vector-expand i array size)))
    array))

;; Inserting nodes can be thought of as counting in base K for a K-way
;; tree. The ith digit ticks over every K^i numbers (so if we've
;; just counted the number N, the digits that have changed are all i
;; where N MOD K^i = 0). In our case, the largest changed digit shares
;; a parent and needs to be pushed onto a node, while the rest will be
;; freshly-allocated.
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

(defun copy-persistent-array (arr)
  ;; Shallow copy (intentional, nodes will be copied as part of traversal
  (make-instance 'persistent-array
                 :root (array-root arr)
                 :tail (array-tail arr)
                 :size (array-size arr)
                 :height (array-height arr)
                 :node-bits (array-node-bits arr)))

(defun copy-path (node key node-height node-bits &key (end-height 1) (copier #'alexandria:copy-array))
  (check-type node vector)
  (check-type key unsigned-byte)
  (check-type node-height (integer 1))
  (check-type node-bits fixnum)
  (check-type end-height unsigned-byte)
  (let* ((node (funcall copier node))
         (new-root node))
    (loop for h from node-height downto (1+ end-height)
       do (let ((idx (key-partition key h node-bits)))
            (setf (elt node idx) (funcall copier (elt node idx)))
            (setf node (elt node idx))))
    (values new-root node)))

(defun tail-items (items node-size)
  (check-type items unsigned-byte)
  (check-type node-size unsigned-byte)
  (if (= items 0)
      0
      (1+ (mod (1- items) node-size))))

(defun in-tail-p (x items node-size)
  (check-type x unsigned-byte)
  (check-type items unsigned-byte)
  (check-type node-size unsigned-byte)
  (and (< x items)
       (>= x (- items (tail-items items node-size)))))

;; TODO: end-height -> as-height (inserts node as height end-height)
;; TODO: This is a lot of params...
(defun insert-item (key item root node-bits copier start-height &key (end-height 1))
  (labels ((expand (node)
             (let ((node-size (expt 2 node-bits)))
               (if node
                   (vector-expand nil (funcall copier node) node-size)
                   (make-array-node node-size nil))))
           (visit (idx node)
             (if (or (null node)
                     ;; Technically just =
                     (>= idx (fill-pointer node)))
                 (expand node)
                 (funcall copier node))))
    ;; TODO: eliminate this cons
    (let* ((p (make-array-node (expt 2 node-bits) root))
           (parent p))
      (loop for child-height from start-height downto (1+ end-height)
         do (let* ((child-idx (key-partition key (1+ child-height) node-bits))
                   (next-child-idx (key-partition key child-height node-bits))
                   (child (visit next-child-idx (elt parent child-idx))))
              (setf (elt parent child-idx) child
                    parent child)))
      ;; Set the final child
      (let ((item-idx (key-partition key (1+ end-height) node-bits)))
        (setf (elt parent item-idx) item))
      (elt p 0))))

(defun array-add (coll x node-copier parray-copier)
  "Append X to the persistent-array COLL, copying nodes with NODE-COPIER and COLL with PARRAY-COPIER."
  (check-type coll persistent-array)
  (let* ((size (array-size coll))
         (node-size (array-node-size coll))
         (node-bits (array-node-bits coll))
         (height (array-height coll))
         (new-array (funcall parray-copier coll)))
    (cond
      ((= size 0)
       (setf (slot-value new-array 'tail) (make-array-node node-size x)))
      ;; Tail is full
      ((= (mod size node-size) 0)
       (cond
         ((null (array-root coll))
          (setf (slot-value new-array 'root) (array-tail coll)
                (slot-value new-array 'height) 1
                (slot-value new-array 'tail) (make-array-node node-size x)))
         ;; If it's full and needs a new root
         ((= (expt node-size (array-height coll)) (- size node-size))
          (let ((new-root (make-array-node node-size (slot-value new-array 'root)))
                (tail (slot-value new-array 'tail))
                (height (1+ height)))
            (setf (slot-value new-array 'height) height
                  (slot-value new-array 'root)
                  (insert-item (1- size) ; Need a key in the tail, not
                                        ; the new element
                               tail
                               new-root
                               node-bits
                               node-copier
                               height
                               :end-height 1)
                  (slot-value new-array 'tail) (make-array-node node-size x))))
         (t
          (setf (slot-value new-array 'root)
                (insert-item (1- size)
                             (slot-value new-array 'tail)
                             (slot-value new-array 'root)
                             node-bits
                             node-copier
                             height
                             :end-height 1)
                (slot-value new-array 'tail)
                (make-array-node node-size x)))))
      (t
       (let ((new-tail (funcall node-copier (array-tail new-array))))
         (setf (slot-value new-array 'tail)
               (vector-expand x new-tail node-size)))))
    (setf (slot-value new-array 'size) (1+ size))
    new-array))

(defmethod add ((coll persistent-array) x &rest xs)
  ;; Add the first element non-destructively
  (let ((new-coll (array-add coll x #'alexandria:copy-array #'copy-persistent-array)))
    ;; The rest can be added in-place, since only the edge copied by
    ;; the add above will be mutated.
    (loop for x in xs
       do (array-add new-coll x #'identity #'identity))
    new-coll))

(defun array-update (bits root height key val)
  (check-type bits fixnum)
  (check-type root vector)
  (check-type height (integer 1))
  (check-type key unsigned-byte)

  ;; Note that we assume key is within bounds for the array (check is
  ;; done in UPDATE
  (multiple-value-bind (new-root last-child) (copy-path root key height bits)
    (setf (elt last-child (key-partition key 1 bits)) val)
    new-root))

(defmethod update ((coll persistent-array) key val &rest others)
  (flet ((update-root (root key val)
           (array-update (array-node-bits coll)
                         root
                         (array-height coll)
                         key
                         val)))
    (let ((size (array-size coll))
          (node-size (array-node-size coll))
          (new-root (array-root coll))
          (new-coll (copy-persistent-array coll)))
      (loop for (key . rest) on (cons key (cons val others)) by #'cddr
         if (endp rest)
         do (error "UPDATE requires an even number of arguments.")
         if (>= key size)
         do (error "Index ~S too large" key)
         do (if (in-tail-p key size node-size)
                (setf (elt (array-tail coll)
                           (mod key node-size))
                      (car rest))
                (setf new-root (update-root new-root key (car rest)))))
      (setf (slot-value new-coll 'root) new-root)
      new-coll)))

(defmethod lookup ((coll persistent-array) x &rest xs)
  (check-type x unsigned-byte)
  (check-type xs null)

  (when (>= x (array-size coll))
    (error 'type-error
           :expected-type `(integer 0 ,(1- (array-size coll)))
           :datum x))
  (let ((node-size (array-node-size coll))
        (size (array-size coll)))
    (if (in-tail-p x size node-size)
        ;; Return the tail entry if it's in the tail
        (elt (array-tail coll) (mod x node-size))
        ;; Otherwise traverse the tree
        (let ((node (array-root coll))
              (bits (array-node-bits coll)))
          (loop for h from (array-height coll) downto 1
             do (setf node (elt node (key-partition x h bits))))
          node))))
