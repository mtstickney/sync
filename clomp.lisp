;;;; clomp.lisp

(in-package #:clomp)

(defclass com-interface ()
  ((vtable :initform nil :accessor interface-vtable)
   ;; TODO: use the the length of the dispatch table for this.
   (method-count :initform nil :accessor method-count)
   (dispatch-table :initform nil :accessor dispatch-table
                   :documentation "A table of method generic functions
                   for this interface. Used to produce and dispatch
                   DISPID-based method calls in the IDispatch
                   interface.")
   (iid :initform nil :accessor interface-iid)
   (parent :initform nil :accessor parent-interface)))

(declaim (inline interface-pointer))
(defun interface-pointer (interface)
  (interface-vtable interface))

(defgeneric satisfies-interface-p (interface iid)
  (:method ((interface com-interface) iid)
    (or (equalp (interface-iid interface) iid)
        (and (parent-interface interface)
             (satisfies-interface-p (parent-interface interface) iid)))))

;; Note that this will prevent interfaces from being GCed. This is
;; probably ok.
(defvar *interface-cache* (make-hash-table :test 'eq))

(defun get-com-interface (interface)
  (check-type interface (or symbol class))
  (unless (subtypep interface 'com-interface)
    (error "Interface ~S is not a subtype of ~S." interface 'com-interface))
  (or (gethash interface *interface-cache*)
      (setf (gethash interface *interface-cache*)
            (make-instance interface))))

(defun parse-uuid (uuid)
  (check-type uuid string)
  (uuid:uuid-to-byte-array (uuid:make-uuid-from-string (subseq uuid 1 (1- (length uuid))))))

(defvar *pointer-to-instance-map* (make-hash-table))

(defun register-instance (instance)
  (check-type instance com-object)
  (let ((address (cffi:pointer-address (%com-instance-pointer instance))))
    (setf (gethash address *pointer-to-instance-map*) instance)))

(defun unregister-instance (instance)
  (check-type instance com-object)
  (let ((address (cffi:pointer-address (%com-instance-pointer instance))))
    (remhash address *pointer-to-instance-map*)))

(defun get-com-instance (pointer)
  (check-type pointer cffi:foreign-pointer)
  (or (gethash (cffi:pointer-address pointer) *pointer-to-instance-map*)
      (error "There is no COM instance object for pointer ~S." pointer)))

(defmacro define-interface (name (iid &optional parent) &body methods)
  (let* ((method-forms (loop for (name return-type . args) in methods
                          collect `(defgeneric ,name (obj ,@(mapcar #'first args)))))
         (method-names (map 'vector #'second method-forms))
         (callback-names (loop for (name . rest) in methods
                            collect (gensym (symbol-name name))))
         (callback-forms (loop for (method-name return-type . args) in methods
                            for callback-name in callback-names
                            for arg-names = (mapcar #'first args)
                            for this-arg = (gensym "THIS")
                            collect `(cffi:defcallback (,callback-name :convention :stdcall) ,return-type
                                         ,(cons `(,this-arg :pointer) args)
                                       (format *debug-io* "In callback ~S~%" ',callback-name)
                                       (,method-name (get-com-instance ,this-arg) ,@arg-names)))))
    `(progn
       ,@method-forms
       ,@callback-forms
       (defclass ,name (com-interface) ())
       (defmethod initialize-instance :after ((obj ,name) &key &allow-other-keys)
         ,@(when parent
             `((setf (parent-interface obj) (get-com-interface ',parent))))
         (let ((parent-method-count (if (parent-interface obj)
                                        (method-count (parent-interface obj))
                                        0))
               (parent-dispatch-table (if (parent-interface obj)
                                          (dispatch-table (parent-interface obj))
                                          nil))
               (parent-vtable (if (parent-interface obj)
                                  (interface-vtable (parent-interface obj))
                                  nil)))
           (setf (method-count obj) (+ ,(length methods) parent-method-count)
                 (interface-iid obj) ,(if iid
                                         `(parse-uuid ,iid)
                                         `(uuid:uuid-to-byte-array (uuid:make-v4-uuid)))
                 (dispatch-table obj) (concatenate 'vector parent-dispatch-table ,method-names)
                 (interface-vtable obj)
                 (cffi:foreign-alloc :pointer
                                     :count ,(length methods)
                                     :initial-contents (list ,@(loop for cb-name in callback-names
                                                                  collect `(cffi:callback ,cb-name)))))
           ;; Initialize parent vtable entries.
           (dotimes (i parent-method-count)
             (setf (cffi:mem-aref (interface-vtable obj) :pointer i)
                   (cffi:mem-aref parent-vtable :pointer i)))
           ;; Initialize local vtable entries.
           (loop for cb-name in ',callback-names
              for i from 0
              do (setf (cffi:mem-aref (interface-vtable obj) :pointer (+ i parent-method-count))
                       (cffi:get-callback cb-name))))
         (trivial-garbage:finalize obj (lambda ()
                                         (format *debug-io* "Finalizing interface~%")
                                         (cffi:foreign-free (interface-vtable obj))))))))

(define-interface iunknown
    ("{00000000-0000-0000-C000-000000000046}")
  (query-interface error-code
                   (iid guid)
                   (obj (:pointer :pointer)))
  (add-ref :ulong)
  (release :ulong))

(cffi:defcstruct com-instance
  (interface-pointer :pointer))

(defclass com-object ()
  ((interfaces :initarg :interfaces :accessor com-interfaces)
   (instance-pointer :initform nil :accessor %com-instance-pointer)
   (clsid :initarg :clsid :accessor class-id
          :allocation :class)
   (ref-count :initform 0 :accessor ref-count))
  (:default-initargs
   :interfaces '(iunknown)
    :clsid nil))

(defun %internal-class-id (clsid)
  (cond
    ((null clsid) (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))
    ((stringp clsid)
     (uuid:uuid-to-byte-array (uuid:make-uuid-from-string clsid)))
    ((typep clsid '(vector (unsigned-byte 8) 16))
     clsid)
    (t (error "Don't know how to process class id value ~S." clsid))))

(defmethod initialize-instance :after ((obj com-object) &key &allow-other-keys)
  (when (> (length (com-interfaces obj)) 1)
    (error "Multiple-interface objects are not yet supported."))
  (when (= (length (com-interfaces obj)) 0)
    (error "COM object must implement at least one interface."))
  (let* ((interface (get-com-interface (first (com-interfaces obj))))
         (interface-pointer (interface-pointer interface))
         (instance-pointer (cffi:foreign-alloc '(:struct com-instance)
                                               :initial-contents `((interface-pointer
                                                                    ,interface-pointer)))))
    (setf (%com-instance-pointer obj) instance-pointer
          (class-id obj) (%internal-class-id (class-id obj)))
    (register-instance obj)
    (trivial-garbage:finalize obj (lambda ()
                                    (format *debug-io* "Finalizing instance.~%")
                                    (cffi:foreign-free (%com-instance-pointer obj))
                                    ;; Note that this will generally
                                    ;; be done by RELEASE, since the
                                    ;; table isn't weak.
                                    (unregister-instance obj)))))

(defmethod satisfies-interface-p ((obj com-object) iid)
  (dolist (interface (com-interfaces obj))
    (let ((iface-obj (get-com-interface interface)))
      (when (satisfies-interface-p iface-obj iid)
        (return-from satisfies-interface-p iface-obj))))
  nil)

(defmethod add-ref ((obj com-object))
  (clomp.ffi:add-ref-server-process) ; increment a global count.
  (incf (ref-count obj)))

(defmethod release ((obj com-object))
  (let ((count (decf (ref-count obj))))
    (when (= count 0)
      (unregister-instance obj))
    (when (= (clomp.ffi:release-server-process) 0)
      ;; FIXME: do something legit here.
      (format *debug-io* "Should shut down now.~%"))
    count))

(defmethod query-interface ((obj com-object) iid ptr)
  (check-type ptr cffi:foreign-pointer)
  (cond
    ((cffi:null-pointer-p ptr)
     :error-invalid-arg)
    ((satisfies-interface-p obj iid)
     (setf (cffi:mem-aref ptr :pointer) (%com-instance-pointer obj))
     (add-ref obj)
     :no-error)
    (t (setf (cffi:mem-aref ptr :pointer) (cffi:null-pointer))
       :error-no-interface)))

(define-interface iclassfactory
    ("{00000001-0000-0000-C000-000000000046}" iunknown)
  (create-instance hresult
                   (outer :pointer)
                   (iid guid)
                   (result (:pointer :pointer)))
  (lock-server hresult
               (lock bool)))

(defclass class-factory (com-object)
  ((server-atom :initform nil :accessor server-atom))
  (:default-initargs
   :interfaces '(iclassfactory)))

(defgeneric unregister-factory (obj)
  (:method ((obj class-factory))
    (when (server-atom obj)
      ;; SBCL has an FFI-related bug here, so ignore-errors it.
      (ignore-errors (clomp.ffi:revoke-class-object (server-atom obj)))
      (setf (server-atom obj) nil))))

(defgeneric register-factory (obj)
  (:method ((obj class-factory))
    (when (server-atom obj)
      (unregister-factory obj))
    ;; FIXME: should allow setting the context and flags.
    (setf (server-atom obj) (clomp.ffi:register-class-object obj))))

(defmethod lock-server ((obj class-factory) lock)
  (if lock
      (clomp.ffi:add-ref-server-process)
      (when (zerop (clomp.ffi:release-server-process))
        ;; FIXME: do something legit here.
        (format *debug-io* "Should shut down now.~%"))))
