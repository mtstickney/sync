(defpackage #:clomp.ffi
  (:use #:cl #:clomp.types)
  (:export #:com-error
           #:register-class-object
           #:revoke-class-object
           #:add-ref-server-process
           #:release-server-process
           #:get-class-object
           #:initialize-com
           #:uninitialize-com))

(in-package #:clomp.ffi)

(cffi:define-foreign-library ole32
  ((:or :windows :mswin32 :mswindows) (:default "ole32")))

(cffi:use-foreign-library ole32)

(define-condition com-error (error)
  ((code :initarg :code :accessor error-code))
  (:report (lambda (c s)
             (format s "COM error: ~S~%" (error-code c)))))

(cffi:defcfun (%register-class-object "CoRegisterClassObject" :convention :stdcall)
    hresult
  (clsid guid)
  (obj :pointer)
  (context class-context)
  (flags register-flags)
  (atom (:pointer dword)))

(defun register-class-object (obj &optional (context :local-server) (flags :multiple-use))
  (check-type obj com-object)
  (cffi:with-foreign-object (atom-ptr 'dword)
    (let ((result (%register-class-object (class-id obj)
                                          (%com-instance-pointer obj)
                                          context
                                          flags
                                          atom-ptr)))
      (unless (zerop result)
        (error 'com-error :code result))
      (cffi:mem-aref atom-ptr 'dword))))

(cffi:defcfun (%revoke-class-object "CoRevokeClassObject" :convention :stdcall)
    hresult
  (atom dword))

(defun revoke-class-object (atom)
  (let ((result (%revoke-class-object atom)))
    (unless (zerop result)
      (error 'com-error :code result))
    (values)))

(cffi:defcfun (add-ref-server-process "CoAddRefServerProcess" :convention :stdcall)
    :ulong)

(cffi:defcfun (release-server-process "CoReleaseServerProcess" :convention :stdcall)
    :ulong)

(cffi:defcfun (%get-class-object "CoGetClassObject" :convention :stdcall)
    hresult
  (clsid guid)
  (context class-context)
  (server-info :pointer)
  (iid guid)
  (out (:pointer :pointer)))

(defun get-class-object (clsid iid &optional (context :inproc-server))
  (check-type clsid (or string (vector (unsigned-byte 8))))
  (check-type iid (or string (vector (unsigned-byte 8))))
  (let ((clsid (if (stringp clsid) (parse-uuid clsid) clsid))
        (iid (if (stringp iid) (parse-uuid iid) iid)))
    (cffi:with-foreign-object (out-ptr :pointer)
      (let ((result (%get-class-object clsid context (cffi:null-pointer) iid out-ptr)))
        (unless (zerop result)
          (error 'com-error :code result))
        (cffi:mem-aref out-ptr :pointer)))))

(cffi:defcfun (%initialize-com "CoInitialize" :convention :stdcall)
    hresult
  (reserved :pointer))

(cffi:defbitfield (coinit dword)
  (:apartment-threaded #x02)
  (:multithreaded #x00)
  (:disable-ole1-dde #x04)
  ;; Turns out this one does nothing.
  (:speed-over-memory #x08))

(cffi:defcfun (%initialize-com* "CoInitializeEx" :convention :stdcall)
    hresult
  (reserved :pointer)
  (flags coinit))

(defun initialize-com (&key threaded)
  "Initialize COM apartments by calling CoInitialize or
CoInitializeEx. If THREADED is true, an STA will be created for the
thread, otherwise the MTA will be initialized."
  (let ((result (if threaded
                    (%initialize-com (cffi:null-pointer))
                    (%initialize-com* (cffi:null-pointer) :multithreaded))))
    (unless (or (zerop result) (= result 1))
      (error 'com-error :code result))
    (if (zerop result)
        t
        ;; Already initialized.
        nil)))

(cffi:defcfun (uninitialize-com "CoUninitialize" :convention :stdcall) :void)
