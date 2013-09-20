;;;; cl-persist.lisp

(in-package #:cl-persist)

;; Operation classes
;; presence
;; key lookup
;; (reverse) iteration
;; (reverse) linearization
;; ordered-selection (nth item in linearization)
;; subset/subsequence
;; size check
;; at-end insertion/deletion
;; key association and dissociation (key addition and key removal)


;; Clojure funcs:
;; count -- size
;; seq -- seq
;; conj -- add
;; peek -- peek
;; pop -- un-add
;; get
;; nth
;; rseq
;; assoc -- link
;; subvec
;; replace(?)
;; hash-map
;; sorted-map
;; sorted-map-by
;; dissoc -- unlink
;; select-keys
;; merge
;; merge-with
;; zipmap
;; contains? -- has-p
;; find
;; keys
;; vals
;; hash-set
;; sorted-set
;; set
;; disj -- without

;;; Public API for generic collections
(defgeneric size (structure)
  (:documentation "Return the number of entries in STRUCTURE."))

;; adjoin/disjoin, maybe?
(defgeneric add (collection x &rest xs) ; conj
  (:documentation "Return a collection with an item described by X and XS added."))

(defgeneric un-add (collection) ; pop
  (:documentation "Return a new collection that is COLLECTION without the last ADDed element. Signals an error for an empty collection."))

(defgeneric peek (collection) ; peek
  (:documentation "Return the last ADDed element of COLLECTION. Returns (values nil nil) for an empty collection, (values x t) otherwise."))

(defgeneric seq (collection) ; seq
  (:documentation "Return a sequence for the elements in COLLECTION."))

;; Note: applies to map-like things, e.g. map & vector
(defgeneric link (collection key val &rest xs) ; assoc
  (:documentation "Return a new collection with KEY associated with VAL (as well as alternating key/val pairs in XS)."))

;; Note: only for maps
(defgeneric unlink (collection key &rest keys) ; dissoc
  (:documentation "Return a new collection without the association for KEY(s)."))

(defgeneric lookup (collection x &rest xs)
  (:documentation "Return the element of COLLECTION described by X and XS."))

;;; Persistent Array container

(defclass persistent-array ()
  ((root :initarg :root
         :reader array-root)
   (tail :initarg :tail
         :reader array-tail)
   (size :initarg :size
         :reader array-size)
   (height :initarg :height
           :reader array-height)
   (node-bits :initarg :node-bits
              :reader array-node-bits)
   (node-size :reader array-node-size))
  (:default-initargs
   :root nil
    :tail nil
    :size 0
    :height 0
    :node-bits 5))

;; TODO: trim the extra junk out of here (and the class def)
(defmethod initialize-instance :after
    ((arr persistent-array) &key node-bits &allow-other-keys)
  (check-type node-bits (integer 1))
  (setf (slot-value arr 'node-size) (expt 2 node-bits)))

(defmethod size ((array persistent-array))
  (array-size array))

(defun print-array-node (node stream)
  (print-object node stream))

(defmethod print-object ((object persistent-array) stream)
  ;; TODO: replace with something that doesn't depend on TCO (or use
  ;; recursion without it)
  ;; TODO: Use the Pretty-Printer, Luke
  (labels ((print-items (node height)
             (if (= height 0)
                 (format stream "~S, " node)
                 (loop for i across node
                    do (print-items i (1- height))))))
    (format stream "[")
    ;; Only print the node structure if there's more than just the tail
    (when (>  (array-size object) (array-node-size object))
      (print-items (array-root object) (array-height object)))
    (when (array-tail object)
      (loop for item across (array-tail object)
         for i from 1
         with len = (length (array-tail object))
         do (format stream "~S" item)
         if (< i len)
         do (format stream ", ")))
    (format stream "]")))

(defmethod print-object ((object persistent-map) stream)
  ;; TODO: replace with something that doesn't depend on TCO (or use
  ;; recursion without it)
  ;; TODO: Use the Pretty-Printer, Luke
  (labels ((print-items (node height)
             (if (= height 0)
                 (loop for ((key . val) . tail) on node
                    do (format stream "~S: ~S" key val)
                      (when tail
                        (format stream ", ")))
                 (loop for i across (map-node-items node)
                    do (print-items i (1- height))))))
    (format stream "{")
    (print-items (map-root object) (map-height object))
    (format stream "}")))

(defun count-chain (node)
  (labels ((count-height (n i)
             (if (= (length (map-node-items n)) 0)
                 i
                 (count-height (elt (map-node-items n) 0) (1+ i)))))
    (count-height node 1)))

(defun vec->list (coll)
  (check-type coll persistent-array)
  (let ((items '())
        (tail (array-tail coll))
        (root (array-root coll))
        (height (array-height coll)))
    (labels ((push-items (node height)
               (if (= height 0)
                   (push node items)
                   (loop for i from (1- (length node)) downto 0
                      do (push-items (elt node i) (1- height))))))
      ;; push the tail items
      (when tail
        (push-items tail 1))
      (when root
        (push-items root height))
      items)))

(defun test-vec (n)
  (let* ((items (loop for i from 1 to n collect n))
         (vec (make-instance 'persistent-array))
         (single-call (apply #'add vec items))
         (multi-call (reduce #'add items :initial-value vec)))
    (assert (equalp items (vec->list single-call)))
    (assert (equalp items (vec->list multi-call)))))

;;; Persistent Map container
(defclass persistent-map ()
  ((root :initarg :root
         :reader map-root)
   (size :initarg :size
         :reader map-size)
   (height :initarg :height
           :reader map-height)
   (node-bits :initarg :node-bits
              :reader map-node-bits)
   (node-size :reader map-node-size))
  (:default-initargs
   :size 0
    :height 1
    :node-bits 5))

(defmethod initialize-instance :after ((coll persistent-map) &key node-bits &allow-other-keys)
  (check-type node-bits (integer 1))
  (let ((node-size (expt 2 node-bits)))
    (setf (slot-value coll 'node-size) node-size
          (slot-value coll 'root) (make-map-node node-size))))

(defmethod size ((coll persistent-map))
  (map-size coll))
