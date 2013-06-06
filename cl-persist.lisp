;;;; cl-persist.lisp

(in-package #:cl-persist)

;;; Public API for the collections
(defgeneric size (structure)
  (:documentation "Return the number of entries in STRUCTURE."))

(defgeneric update (collection x &rest xs)
  (:documentation "Return a new collection updated with items X and XS. The meaning of X and XS may vary from collection to collection."))

;;; Persistent Array container

(defclass persistent-array ()
  ((root :initform (make-array-node)
         :initarg :root
         :reader array-root)
   (size :initform 0
         :initarg :size
         :reader array-size)))

(declaim (inline index-height))
(defun index-height (index)
  (ceiling (/ (integer-length index) +node-bits+)))

(defun %build-path (max-height index value)
  (let ((node value))
    (loop for i from 1 to max-height
       do (setf node (%build-node i
                                  (level-index index i)
                                  node)))
    node))

(defun tree-insert (root index value)
  (check-type root array-node)
  (check-type index integer)
  (let* ((height (array-node-height root))
         (node-index (level-index index height)))
    (cond
      ((= (array-node-height root) 1)
       (update root node-index value))
      ((not (has-index-p root node-index))
       (update root node-index (%build-path (1- height) index value)))
      (t
       (update root node-index (tree-insert (node-item root node-index)
                                            index
                                            value))))))

;; TODO: replace elt with aref where necessary, add range checks (aref/elt
;; may or may not error on invalid access)
;; For now, vector must be continguous: (insert [a b c] 20 3) will
;; blow up
(defmethod update ((coll persistent-array) index value)
  (check-type index integer)
  (when )
  )

(defun array-insert (array index value)
  (check-type array persistent-array)
  (check-type index integer) ; Global index, can be a bignum
  (let ((index-height (index-height index))
        (tree-height (array-node-height (array-root array)))
        (new-root (array-root array)))
    ;; Create the new superstructure
    (loop for i from (1+ tree-height) to index-height
       do (setf new-root (%build-node i
                                      (level-index index i)
                                      new-root)))
    ;; Now go set

    )
  )
;; TODO: This is broken now that array-leaf-node is out
;; (defgeneric array-push (array item)
;;   (:method ((array persistent-array) item)
;;     (let* ((new (make-instance 'persistent-array :size (1+ (array-size array))))
;;            (root (array-root array)))
;;       (cond
;;         ((and (typep root 'array-leaf-node)
;;               (< (fill-pointer (array-leaf-node-leafs root)) 32))
;;          (setf (slot-value new 'root) (node-push root item)))
;;         ((typep root 'array-leaf-node)
;;          ;; TODO: create a non-leaf root
;;          )
;;         )
;;       new)))

(defmethod size ((array persistent-array))
  (array-size array))

(defun print-array-node (node stream)
  (print-object (array-node-children node) stream))
