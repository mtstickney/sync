;;;; cl-persist.lisp

(in-package #:cl-persist)

;;; Public API for the collections
(defgeneric size (structure)
  (:documentation "Return the number of entries in STRUCTURE."))

;;; Persistent Array container

(defclass persistent-array ()
  ((root :initform (make-array-leaf-node)
         :initarg :root
         :reader array-root)
   (size :initform 0
         :initarg :size
         :reader array-size)))

(defstruct (array-node (:copier nil))
  (children (make-array 32 :fill-pointer 0 :element-type '(or array-node array-leaf-node))))

(defstruct (array-leaf-node (:copier nil))
  (leafs (make-array 32 :fill-pointer 0)))

(defun copy-array-node (node)
  (check-type node array-node)
  (make-array-node :children (alexandria:copy-array (array-node-children node))))

(defun copy-array-leaf-node (node)
  (check-type node array-leaf-node)
  (make-array-leaf-node :leafs (alexandria:copy-array (array-leaf-node-leafs node))))

(defmethod size ((array persistent-array))
  (array-size array))

(defgeneric node-push (node item)
  (:method ((node array-node) item)
    (assert (< (fill-pointer (array-node-children node)) 32) ()
            "Node-push performed on full node.")
    (let ((new (copy-array-node node)))
      (vector-push item (array-node-children new))
      new))
  (:method ((node array-leaf-node) item)
    (assert (< (fill-pointer (array-leaf-node-leafs node)) 32) ()
            "Node-push performed on full node.")
    (let ((new (copy-array-leaf-node node)))
      (vector-push item (array-leaf-node-leafs new))
      new)))

(defgeneric array-push (array item)
  (:method ((array persistent-array) item)
    (let* ((new (make-instance 'persistent-array :size (1+ (array-size array))))
           (root (array-root array)))
      (cond
        ((and (typep root 'array-leaf-node)
              (< (fill-pointer (array-leaf-node-leafs root)) 32))
         (setf (slot-value new 'root) (node-push root item)))
        ((typep root 'array-leaf-node)
         ;; TODO: create a non-leaf root
         )
        )
      new)))

(defun print-leaf-node (node stream)
  (print-object (array-leaf-node-leafs node) stream))
