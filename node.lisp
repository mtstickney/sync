(in-package #:cl-persist)

(defconstant +node-bits+ 5)
(defconstant +node-size+ (expt 2 +node-bits+))

(declaim (type fixnum +node-bits+))
(declaim (type fixnum +node-size+))

(defstruct (array-node (:copier nil))
  (children (make-array 0 :fill-pointer t :adjustable t :element-type 'array-node))
  (height 1))

(defun copy-array-node (node)
  (check-type node array-node)
  (make-array-node :children (alexandria:copy-array (array-node-children node))
                   :height (array-node-height node)))

;; This ensures the doubling behavior (not in the standard), and works
;; well with +NODE-SIZE+ = 32
(defun push-extend (item vector)
  (check-type vector vector)
  (let ((capacity (array-total-size vector)))
    (when (= (fill-pointer vector) capacity)
      (setf vector (adjust-array vector (if (= capacity 0) 1 (* 2 capacity)))))
    (vector-push item vector)))

;; Like (add (make-array-node) x...) but without the initial copy
(defun mk-array-node (height &rest xs)
  (check-type height integer)
  "Build an array-node prepopulated with children XS."
  (let* ((node (make-array-node :height height))
         (children (aray-node-children)))
    (loop for item in (cons x xs)
       do (push-extend item children))
    node))

;; TODO: Do we really need to assert the size in here, or can we just
;; do trust the caller to do the right thing?
(defmethod add ((node array-node) val &rest vals)
  (let* ((new-node (copy-array-node node))
         (children (array-node-children new-node)))
    (loop for item in (cons val vals)
       do (push-extend item children))
    (assert (<= (length children) +node-size+)
            ()
            "Pushed too many items to node ~S, node is over-full."
            new-node)
    new-node))

(defmethod update ((node array-node) x &rest xs)
  (let* ((new-node (copy-array-node node))
         (children (array-node-children new-node)))
    (loop for (index . rest)
       on (cons x xs)
       by #'cddr
       do (let ((val (car rest)))
            (check-type index fixnum)
            (when (endp rest)
              (error "UPDATE expects an even number of arguments."))
            (setf (elt children index) val)))
    new-node))

(defun %build-path (max-height node)
  "Return a tree of height MAX-HEIGHT with NODE at the correct height. All ancestors of NODE will have one child, the leftmost."
  (check-type max-height integer)
  (check-type node)
  (loop for i from (1+ (array-node-height node)) to max-height
     do (setf node (mk-array-node i node)))
  node)

(defun append-node (tree leaf)
  "Attach LEAF as the rightmost leaf in TREE."
  (check-type tree (or array-node null))
  (check-type node (array-node))
  (assert (= (array-node-height leaf) 1) ()
          "~S is not a leaf node, cannot append it to a tree."
          leaf)
  (labels ((tree-append (root node)
             (cond)
             )))
  )
