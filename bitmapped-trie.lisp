(in-package #:cl-persist)

;; This is a story about a class, called a bitmapped-trie:
(defclass bitmapped-trie ()
  ;; This class has some information about a tree, like
  (;; the number of items it holds,
   (size :reader trie-size
         :initarg :size)
   ;; and a tree of nodes to hold the items in the trie.
   (root :reader trie-root
         :initarg :root)
   ;; The height of the trie
   (height :reader trie-height
           :initarg :height)
   ;; The trie also stores the number of key bits that should be used
   ;; to for an index into the node.
   (node-bits :reader trie-node-bits
              :initarg :node-bits))
  (:default-initargs
   ;; The trie initially holds 0 elements
   :size 0
    ;; and is of height 0
    :height 0
    ;; the tree of nodes is initially nil,
    :root nil
    ;; and the number of key bits used for indexing is 5 by default.
    :node-bits 5))

;; Inside the trie is a tree of nodes,
(defstruct (node (:constructor %mk-node))
  ;; and every node has an array of children, which must be adjustable.
  (children #() :type vector)
  ;; The array of children may not be completely allocated if it's not
  ;; full, so a node also stores a maximum number of children.
  (size :type number :read-only t))

;; To get the length (number of inserted elements) of the node, we
;; return the length of the underlying array.
(defun node-length (node)
  (check-type node node)
  (length (node-children node)))

;; All indexes for a node are valid, and since the indexes for nodes
;; are specified as a number of bits, the number of children a node
;; can have must be a power of two. When appending to a node, the
;; vector of children may need to be extended. A common method is to
;; resize a node from size n to 2n+1.
(defun %node-push (item node)
  (check-type node node)
  (flet ((next-size (n)
           (if (= n 0)
               1
               (* 2 n))))
    ;; If the node is already full, we signal an error:
    (when (>= ))
    (let* ((children (node-children node))
           (allocated (array-total-size children)))

      ;; We only need a resize if the children vector is already full,
      (when (>= (length children) allocated)
        (setf (node-children node)
              (adjust-array children
                            ;; and we don't want to allocate any more
                            ;; than we need to.
                            (min (node-size node)
                                 (next-size allocated)))))
      ;; Once the node's been resized (if needed), we can push the
      ;; element onto the vector.
      (vector-push item children))))

;; The trie is responsible for constructing nodes, since each node
;; must have the same number of children.
(defmethod make-node ((trie bitmapped-trie) &rest values)
  (let* ((size (expt 2 (trie-node-bits trie)))
         (children (make-array 0 :adjustable t :fill-pointer 0))
         (node (%mk-node :children children :size size)))
    ;; The trie's node constructor will also pre-populate the node
    ;; with an initial set of values, if any.
    (loop for v in values
       for i from 1
       if (> i size)
       do (error "Too many values for node to hold, need <=~S" size)
       do (%node-push v node))
    node))

;; To get the child of a node in the tree corresponding to a
;; particular key, we must first extract an index for this particular
;; level of the tree.
(defun node-index (node-bits key height)
  ;; If NODE-BITS is the number of bits for a node index, the portion
  ;; of the key we want is the (1- height)th group of NODE-BITS bits,
  ;; from least significant to most. First we construct a mask of
  ;; NODE-BITS bits:
  (logand (1- (ash 1 num-bits))
          ;; Right-shifting the key by (1- height) groups of NODE-BITS
          ;; bits and ANDing with the mask yields our index:
          (ash key (- (* num-bits (1- height))))))

;; Once we can retrieve the index for a node from a key, getting the
;; actual child is just a matter of using that index to retrieve an
;; element of the underlying array.
(defun node-child (node key height)
  (check-type node node)
  (let ((index (node-index (log (node-size node) 2) key height)))
    (elt (node-children node) index)))
