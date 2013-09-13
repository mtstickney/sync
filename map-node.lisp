(in-package #:cl-persist)

(defun copy-persistent-map (map)
  "Return a shallow-copy of the PERSISTENT-MAP instance MAP."
  (check-type map persistent-map)
  (make-instance 'persistent-map
                 :root (map-root map)
                 :size (map-size map)
                 :height (map-height map)
                 :node-bits (map-node-bits map)))

(defstruct (map-node (:constructor mk-map-node))
  (map nil :type simple-bit-vector :read-only t)
  (items nil :type vector))

(define-condition invalid-index-error ()
  ((idx :initarg :idx
        :reader index)))

(define-condition missing-index-error (invalid-index-error)
  ()
  (:report (lambda (c s)
             (format s "Sparse index ~S is invalid." (index c)))))

(define-condition out-of-bounds-index-error (invalid-index-error)
  ()
  (:report (lambda (c s)
             (format s "Index ~S is too large." (index c)))))

(defun has-item (node idx)
  (check-type node map-node)
  (check-type idx unsigned-byte)
  (let ((bitmap (map-node-map node)))
    (and (< idx (length bitmap))
         (= (elt bitmap idx) 1))))

(defun element (node idx)
  (check-type node map-node)
  (check-type idx unsigned-byte)
  (let ((bitmap (map-node-map node)))
    (unless (< idx (length bitmap))
      (error 'out-of-bounds-index-error :idx idx))
    (unless (= (elt bitmap idx) 1)
      (error 'missing-index-error :idx idx))
    (let ((item-idx (count 1 bitmap :end idx)))
      (elt (map-node-items node) item-idx))))

(defun (setf element) (val node idx)
  (check-type node map-node)
  (check-type idx unsigned-byte)
  (let ((bitmap (map-node-map node))
        (items (map-node-items node)))
    (unless (< idx (length bitmap))
      (error 'out-of-bounds-index-error :idx idx))
    (let* ((item-idx (count 1 bitmap :end idx))
           (new-items (if (< item-idx (length items))
                          (progn
                            (setf (elt items item-idx) val)
                            items)
                          (vector-expand val items (length bitmap)))))
      (setf (map-node-items node) new-items
            (elt bitmap idx) 1)
      val)))

(define-condition odd-arguments-error () ()
  (:report "Odd number of arguments."))

(defun make-map-node (size &rest items)
  (let ((node (mk-map-node :map (make-array size
                                            :element-type 'bit
                                            :initial-element 0)
                           :items (make-array size
                                              :fill-pointer 0
                                              :adjustable t))))
    (loop for (idx . rest) on items by #'cddr
       do (when (null rest)
            (error 'odd-arguments-error))
         (setf (element node idx) (car rest)))
    node))

(defmethod lookup ((coll persistent-map) x &rest xs)
  (check-type xs null)
  (let ((hash (sxhash x))
        (node (map-root coll))
        (bits (map-node-bits coll)))
    (handler-case
        (loop for h from (map-height coll) downto 1
           do (setf node (element node (key-partition hash h bits))))
      (invalid-index-error (c)
        (declare (ignore c))
        (return-from lookup (values nil nil))))
    ;; node is now the list bucket
    (let ((entry (assoc x node :test #'equal)))
      (if entry
          (values (cdr entry) t)
          (values nil nil)))))

(defun max-partition (key partition-bits)
  ;; We always require at least one partition
  (max 1
       (ceiling (integer-length key) partition-bits)))

(defun reparent (root height hash partition-bits)
  "Return a (possibly new) tree node that is tall enough to support KEY."
  (let ((new-height (max-partition hash partition-bits))
        (node-size (length (map-node-map root))))
    (loop with node = root
       for h from height to new-height
       do (setf node (make-map-node node-size 0 node))
       finally (return (values node (max height new-height))))))

(defun insert-key (root height hash key val partition-bits &key copier)
  (check-type root map-node)
  (check-type hash fixnum)
  (let ((new-root (funcall copier root))
        (node-size (length (map-node-map root)))
        (added-item nil))
    (loop with node = new-root
       for h from (1- height) downto 1
       do (let* ((idx (key-partition hash h partition-bits))
                 (new-child (if (has-item node idx)
                                (funcall copier (element node idx))
                                (make-map-node node-size))))
            (setf (element node idx) new-child
                  node new-child))
       finally (let* ((idx (key-partition hash 0 partition-bits))
                      (chain (element node idx)))
                 (setf (element node idx)
                       (cond
                         ((not (has-item node idx))
                          (setf added-item t)
                          (acons key val nil))
                         ((assoc key chain :test #'eql)
                          (mapcar (lambda (c)
                                    (if (eql (car c) key)
                                        (cons key val)
                                        c))
                                  chain))
                         (t
                          (setf added-item t)
                          (acons key val chain))))))
    (values new-root added-item)))

(defun add-item% (coll key val)
  (let* ((root (map-root coll))
         (bits (map-node-bits coll))
         (node-size (map-node-size coll))
         (height (map-height coll))
         (hash (sxhash key)))
    ;; Note: after adding parents to extend the height of root to
    ;; (MAX-PARTITION key), Node copying needs to be done iff the
    ;; new root is EQ to the old root.
    ;; Justification: New parents are added to the root iff the key
    ;; has more digits than any key already in root. If this is the
    ;; case, the digits beyond the largest key in root form a prefix
    ;; for a key space (different subtree) separate from the root, and
    ;; no copying must be done. If the key has the same or fewer
    ;; digits than the largest key, then it shares a prefix with the
    ;; root, is thus in the same key space (subtree) as the nodes in
    ;; root, and thus nodes must be copied to avoid modification (the
    ;; root node, at minimum).
    (multiple-value-bind (new-root new-height) (reparent root height key bits)

      (multiple-value-bind (new-root added-p)
          (insert-key new-root new-height key val bits
                      :copier (if (eq new-root root)
                                  #'copy-map-node
                                  #'identity))
        (with-slots (root size height) coll
          (setf root new-root
                height new-height)
          (when added-p
            (incf size)))))
    (values)))

(defmethod add ((coll persistent-map) x &rest xs)
  (assert (not (null xs)) (xs)
          "ADD requires at least two arguments for a persistent-map.")
  (let ((new-coll (copy-persistent-map coll)))
    (add-item% new-coll x (car xs))
    (loop for (key . rest) on xs by #'cddr
       if (null rest)
       do (error "ADD requires an even number of arguments.")
       do (add-item% new-coll key (car rest)))
    new-coll))
