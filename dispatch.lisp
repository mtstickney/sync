(defpackage #:clomp.dispatch
  (:use #:cl #:clomp.types #:clomp)
  (:export #:idispatch
           #:dispatchable-com-object))

(in-package #:clomp.dispatch)

(cffi:defcenum (invoke-type word)
  (:method #x01)
  (:property-get #x02)
  (:property-put #x04)
  (:property-put-ref #x08))

(define-interface idispatch
    ("{00020400-0000-0000-C000-000000000046}" iunknown)
  (get-type-info-count hresult
                       (count (:pointer :uint)))
  (get-type-info hresult
                 (index :uint) ; must be 0
                 (locale-id dword)
                 (type-info :pointer))
  (get-ids-of-names hresult
                    (riid :pointer) ; guid pointer, unused (should be NULL)
                    (names (:pointer (:string :encoding #.+utf16-encoding+)))
                    (names-count :uint)
                    (locale-id dword)
                    (dispids (:pointer dispid)))
  (invoke hresult
          (member dispid)
          (riid :pointer) ; guid pointer, unused (should be NULL)
          (locale-id dword)
          (invoke-type invoke-type)
          (params (:pointer (:struct disp-params)))
          (result (:pointer variant))
          (exception-info (:pointer (:struct excep-info)))
          (bad-arg-index (:pointer :uint))))

(defclass dispatchable-com-object (com-object)
  ()
  (:default-initargs
   :interfaces '(idispatch)))

(defmethod get-type-info-count ((obj dispatchable-com-object) count)
  ;; We're going to claim that we don't support type information.
  (setf (cffi:mem-aref count :uint) 0)
  0)

(defmethod get-ids-of-names ((obj dispatchable-com-object) riid names names-count locale-id dispids)
  (declare (ignore riid locale-id))
  (when (> names-count 1)
    (dotimes (i (1- names-count))
      (setf (cffi:mem-aref dispids 'dispid (1+ i))
            (cffi:foreign-enum-value 'dispid-constant :dispid-unknown)))
    (return-from get-ids-of-names (cffi:foreign-enum-value 'dispid-constant :dispid-unknown-name)))

  (when (= names-count 0)
    ;; I guess we're done?
    (return-from get-ids-of-names 0))

  ;; FIXME: broken for multiple interfaces (which aren't supported anyway)
  (loop with name = (cffi:mem-aref names `(:string :encoding ,+utf16-encoding+))
     for func across (clomp::dispatch-table (clomp::get-com-interface (first (clomp::com-interfaces obj))))
     for dispid upfrom 1
     when (equalp name (symbol-name func))
     do (setf (cffi:mem-aref dispids 'dispid) dispid)
       (return 0)
     finally (setf (cffi:mem-aref dispids 'dispid)
                   (cffi:foreign-enum-value 'dispid-constant :dispid-unknown))
       (return (cffi:foreign-enum-value 'dispid-constant :dispid-unknown-name))))

(defun fill-exception-info (c exception-info)
  (check-type c condition)
  (check-type exception-info cffi:foreign-pointer)
  (when (cffi:null-pointer-p exception-info)
    (return-from fill-exception-info (values)))

  (cffi:with-foreign-slots ((clomp.types::error-code
                             clomp.types::reserved
                             clomp.types::error-source
                             clomp.types::error-description
                             clomp.types::help-file-path
                             clomp.types::help-context
                             clomp.types::scode)
                            exception-info
                            (:struct excep-info))
    ;; id for internal errors (made-up)
    (setf clomp.types::error-code 1
          clomp.types::reserved 0
          clomp.types::error-source "CLOMP COM bridge."
          ;; TODO: add a backtrace.
          clomp.types::error-description (format nil "~A" c)
          clomp.types::help-file-path (cffi:null-pointer)
          clomp.types::help-context 0
          clomp.types::scode 0))
  (values))

(defmacro with-com-error-handlers ((exception-info) &body body)
  `(block com-handler-block
     (handler-bind
         ((serious-condition (lambda (c)
                               (fill-exception-info c ,exception-info)
                               (return-from com-handler-block (cffi:foreign-enum-value 'dispid-constant
                                                                                       :dispid-exception)))))
       ,@body)))

(defun params-count (params)
  (check-type params cffi:foreign-pointer)
  (cffi:foreign-slot-value params '(:struct clomp.types:disp-params) 'clomp.types:arg-count))

(defun named-params-count (params)
  (check-type params cffi:foreign-pointer)
  (cffi:foreign-slot-value params '(:struct clomp.types:disp-params) 'clomp.types:named-arg-count))

(defun param-ptr (params i)
  (check-type params cffi:foreign-pointer)
  (check-type i (integer 0))
  (cffi:mem-aptr (cffi:foreign-slot-pointer params '(:struct clomp.types:disp-params) 'clomp.types:args)
                 'clomp.types:variant
                 i))

(defun param-arg (params i)
  (check-type params cffi:foreign-pointer)
  (check-type i (integer 0))
  (cffi:mem-aref (param-ptr params i) 'clomp.types:variant))

(defun ref-arg-p (variant-ptr)
  (check-type variant-ptr cffi:foreign-pointer)
  (let ((type (cffi:foreign-slot-value variant-ptr '(:struct clomp.types::variant-struct) 'clomp.types::type)))
    (member :byref (rest type))))

(defun (setf param-arg) (val params i &key (errorp t))
  (check-type params cffi:foreign-pointer)
  (check-type i (integer 0))
  (let ((ptr (param-ptr params i)))
    (cond
      ((and errorp (not (ref-arg-p ptr)))
       (error "Cannot set a non-ref argument."))
      ((ref-arg-p ptr)
       (setf (cffi:mem-aref ptr 'variant) val)))))

(defun value-arg-p (params)
  (and (= (params-count params) 1)
       (= (named-params-count params) 1)
       (= (cffi:mem-aref (cffi:foreign-slot-value params
                                                  '(:struct clomp.types:disp-params)
                                                  'clomp.types:named-arg-ids)
                         'dispid
                         0)
          (cffi:foreign-enum-value 'clomp.types:dispid-constant :dispid-value))))

(defun param-list (params)
  (check-type params cffi:foreign-pointer)
  (when (and (> (named-params-count params) 0)
             (not (value-arg-p params)))
    (error "Named arguments are not supported."))
  ;; Parameters are in reverse order.
  (loop with args = '()
     for i from 0 to (1- (params-count params))
     do (push (param-arg params i) args)
     finally (return args)))

(defun set-params (params vals)
  (check-type params cffi:foreign-pointer)
  (let ((count (params-count params))
        (i 0))
    (assert (= (length vals) count))
    (dolist (val vals)
      ;; Params are in reverse order.
      (setf (param-arg params (- count i 1) :errorp nil)
            val)
      (incf i))))

(defmethod invoke ((obj dispatchable-com-object) member riid locale-id invoke-type params result exception-info bad-arg-index)
  (declare (ignore locale-id))
  (check-type member (integer 1))
  (check-type riid cffi:foreign-pointer)
  (check-type invoke-type keyword)
  (check-type params cffi:foreign-pointer)
  (check-type result cffi:foreign-pointer)
  (check-type exception-info cffi:foreign-pointer)
  (check-type bad-arg-index cffi:foreign-pointer)
  (unless (cffi:null-pointer-p riid)
    (return-from invoke (cffi:foreign-enum-value 'dispid-constant :dispid-unknown-interface)))

  (with-com-error-handlers (exception-info)
    ;; TODO: do the rest of the stuff with the things.
    (let* ((interface (clomp::get-com-interface (first (clomp::com-interfaces obj))))
           (method (progn (when (> (1- member) (length (clomp::dispatch-table interface)))
                            (return-from invoke (cffi:foreign-enum-value 'dispid-constant
                                                                         :dispid-member-not-found)))
                          (aref (clomp::dispatch-table interface) member))))
      (when (> (cffi:foreign-slot-value params '(:struct clomp.types:disp-params) 'clomp.types:named-arg-count)
               0)
        (return-from invoke (cffi:foreign-enum-value 'dispid-constant :dispid-no-named-args)))
      (ecase invoke-type
        ((:member :property-get)
         (multiple-value-bind (val inner-type)
             (apply method (cons obj (param-list params)))
           (unless (cffi:null-pointer-p result)
             (when (null inner-type)
               (error "Variant type for value ~S was not reported, cannot convert to return object."
                      val))
             (let ((type  `(clomp.types:variant ,(if (keywordp inner-type)
                                                     (list inner-type)
                                                     inner-type))))
               (cffi:convert-into-foreign-memory val type result)))))
        (t (error "Only method calls and property retrieval are supported."))))))
