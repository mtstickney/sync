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
   (node-size :reader array-node-size)
   (key-mask :reader array-key-mask))
  (:default-initargs
   :root nil ;;(make-array-node)
    :tail nil
    :size 0
    :height 0
    :node-bits 5))

;; TODO: trim the extra junk out of here (and the class def)
(defmethod initialize-instance :after
    ((arr persistent-array) &key node-bits &allow-other-keys)
  (check-type node-bits (integer 1))
  (setf (slot-value arr 'node-size) (expt 2 node-bits)
        (slot-value arr 'key-mask) (1- (ash 1 node-bits))))

(defmethod size ((array persistent-array))
  (array-size array))

(defun print-array-node (node stream)
  (print-object node stream))

(defmethod print-object ((object persistent-array) stream)
  ;; TODO: replace with something that doesn't depend on TCO (or use
  ;; recursion without it)
  (labels ((print-items (node height)
             (if (= height 0)
                 (format stream "~S, " node)
                 (loop for i across node
                    do (print-items i (1- height))))))
    (format stream "[")
    (when (>  (array-size object) 0)
      (print-items (array-root object) (array-height object)))
    (format stream "]")))
