(in-package #:cl-persist)

(defconstant +node-bits+ 5)
(defconstant +node-length+ (expt 2 +node-bits+))

(declaim (type +node-length+ fixnum))
(declaim (type +node-bits+ fixnum))

(defstruct (array-node (:copier nil))
  (bitmap (make-array +node-length+ :element-type 'bit :initial-element 0))
  (children (make-array +node-length+ :fill-pointer 0 :element-type 'array-node))
  (height 1))

(defun copy-array-node (node)
  (check-type node array-node)
  (make-array-node :children (alexandria:copy-array (array-node-children node))
                   :bitmap (alexandria:copy-array (array-node-bitmap node))
                   :height (array-node-height node)))

;; Like update, but without having to create a copy of the initial
;; empty node
(defun %build-node (height &rest indices)
  (when (oddp (length indexes))
    ;; TODO: make a proper condition for this
    (error "Object of type ARRAY-NODE requires an even number of arguments for %BUILD-NODE"))
  (let ((node (make-array-node :height height)))
    (loop for (index . (value . rest))
       on indices
       by #'cddr
       do (%update-node node index value))
    node))

(defun has-index-p (node level-index)
  "Primary value is T if NODE has an entry for LEVEL-INDEX, NIL otherwise. Secondary value is NIL if LEVEL-INDEX is out-of-bounds for this node, T otherwise."
  (check-type node array-node)
  (check-type level-index fixnum)
  (with-slots (bitmap) node
    (if (>= level-index (length bitmap))
        (values nil nil)
        (values (= (sbit bitmap level-index))))))

(defgeneric node-item (node index)
  (:method ((node array-node) index)
    (check-type index fixnum)
    (if (not (has-index-p node index))
        (values nil nil)
        (values (elt (array-node-children node) index)
                t))))

(declaim (inline level-index))
(defun level-index (index height)
  (check-type index integer) ; global index
  (check-type depth fixnum)
  (logand (ash index (- (* +node-bits+ (1- height))))
          (the fixnum #x1F)))

(declaim (inline node-index))
(defun node-index (node level-index)
  (check-type node array-node)
  (check-type level-index fixnum)
  (with-slots (bitmap) node
    ;; This is probably not very efficient. Switch to fixnum?
    ;; Count set bits to the right of position LEVEL-INDEX
    (count 1 bitmap
           :start (1+ (- (the fixnum (length bitmap)) ; => +NODE-LENGTH+ unless something is wrong
                         (the fixnum level-index))))))

;; Note: INDEX and INDEXES are indices into this node, not top-level
;; array indices
(defun %update-node (node index value)
  (check-type node array-node)
  (check-type index fixnum)
  (when (> (array-node-height node) 1)
    (check-type value array-node))
  (with-slots (bitmap children) node
    (setf (sbit bitmap index) 1)
    (setf (elt children index) val)))

(defmethod update ((coll array-node) index &rest indexes)
  (when (evenp (length indexes))
    ;; TODO: make a proper condition for this
    (error "Object of type ARRAY-NODE requires an even number of arguments for UPDATE"))
  (let ((new-node (copy-array-node coll)))
    (with-slots (bitmap children) new-node
      (loop for (index . (val . rest))
         on (cons index indexes)
         by #'cddr
         do (%update-node new-node index val)))
    new-node))
