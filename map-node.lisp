(in-package #:cl-persist)

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

  )

(defmethod add ((coll persistent-map) x &rest xs)
  (let ((root (map-root coll))
        (bits (map-node-bits coll))
        (node-size (map-node-size coll))
        (height (map-height coll))
        )
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
    ()
    )
  )
