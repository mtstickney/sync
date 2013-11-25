;;;; cl-svc.lisp

(in-package #:cl-svc)

(cffi:define-foreign-library advapi32
  ((or :mswindows :win32 :windows) (:default "advapi32")))

(cffi:use-foreign-library advapi32)

(cffi:defcstruct service-table-entry
  "SERVICE_TABLE_ENTRY for Windows services."
  (service-name (:string :encoding :utf-16))
  (service-func :pointer))

(cffi:defcfun (start-service-ctrl-dispatcher "StartServiceCtrlDispatcherW"
                                             :convention :stdcall
                                             :library advapi32)
    :int
  (service-table (:pointer (:struct service-table-entry))))

(cffi:defcenum (service-type :ulong)
  (:file-system-driver #x02)
  (:kernel-driver #x01)
  (:win32-own-process #x10)
  (:win32-shared-process #x20))

(cffi:defcenum (service-state :ulong)
  (:stopped #x01)
  (:start-pending #x02)
  (:stop-pending #x03)
  (:running #x04)
  (:continue-pending #x05)
  (:pause-pending #x06)
  (:paused #x07))

(cffi:defcenum (service-control-set :ulong)
  (:stop #x01)
  (:pause-continue #x002)
  (:shutdown #x004)
  (:param-change #x008)
  (:netbind-change #x010)
  (:pre-shutdown #x100)
  ;; Extended control sets
  (:hardware-profile-change #x020)
  (:power-event #x40)
  (:session-change #x80)
  (:time-change #x200)
  (:trigger-event #x400)
  (:user-mode-reboot #x800))

(defun flags (type &rest values)
  "Return an ORed set of flags for the VALUES of enum TYPE."
  (apply #'logior
         (mapcar (lambda (v)
                   (typecase v
                     (keyword (cffi:foreign-enum-value type v))
                     (t (cffi:foreign-enum-value type (cffi:foreign-enum-keyword type v)))))
                 values)))

(cffi:defcstruct service-status
  "SERVICE_STATUS for Windows services."
  (service-type service-type)
  (status service-state)
  (accepted-controls :ulong)
  (win32-exit-code :ulong)
  (service-specific-exit-code :ulong)
  (checkpoint :ulong) ; progress during initialization/shutdown
  (wait-hint :ulong) ; timeout to wait for a checkpoint
                     ; increment/status change
  )

(cffi:defctype handle :pointer "Handle to in-memory resource.")
(cffi:defctype service-status-handle handle "Handle to a SERVICE-STATUS struct.")
(cffi:defctype sc-handle handle "Handle to a service control manager database.")

(cffi:defcfun (register-service-ctrl-handler "RegisterServiceCtrlHandlerExW"
                                             :convention :stdcall
                                             :library advapi32)
    service-status-handle
  "Register a service handler table for use. Returns a handle that can be used to set the service status."
  (service-name (:string :encoding :utf-16))
  (handler :pointer)
  (data :pointer))

(cffi:defcfun (set-status-service "SetServiceStatus"
                                  :convention :stdcall
                                  :library advapi32)
    :int
  (status-handle service-status-handle)
  (status (:pointer (:struct service-status))))

;;; Code for installing/uninstalling a service
(cffi:defcenum (generic-rights :ulong)
    (:delete #x00010000)
    (:read-control #x00020000)
    (:write-dac #x00040000)
    (:write-owner #x00080000)
    (:synchronize #x00100000)
    (:required #x000F0000)
    (:read #x00020000) ; :read-control
    (:write #x00020000) ; :read-control
    (:execute #x#x00020000) ; :read-control
    (:all #x001F0000)
    (:specific-all #x0000FFFF)
    (:generic-all #x10000000)
    (:generic-execute #x20000000)
    (:generic-write #x40000000)
    (:generic-read #x#x80000000))

(cffi:defcenum (sc-manager-rights)
  (:all-access #xF003F)
  (:create-service #x0002)
  (:connect #x0001)
  (:enumerate-service #x0004)
  (:lock #x0008)
  (:modify-boot-config #x20)
  (:query-lock-status #x10))

(cffi:defcfun (open-sc-manager "OpenSCManagerW"
                               :convention :stdcall
                               :library advapi32)
    sc-handle
  (machine-name (:string :encoding :utf-16))
  (database-name (:string :encoding :utf-16))
  (desired-access :ulong))

(cffi:defcenum (service-start-type :ulong)
  (:auto-start #x02)
  (:boot-start #x00)
  (:demand-start #x03)
  (:disabled #x04)
  (:system-start #x01))

;; Used to indicate the level of error if the service fails to start
(cffi:defcenum (service-error-level :ulong)
  (:critical #x03)
  (:ignore #x00)
  (:normal #x01)
  (:severe #x02))

(cffi:defcfun (create-service "CreateServiceW"
                              :library advapi32
                              :convention :stdcall)
    sc-handle
  (sc-manager sc-handle)
  (service-name (:string :encoding :utf-16))
  (display-name (:string :encoding :utf-16))
  (desired-access :ulong)
  (service-type service-type)
  (start-type service-start-type)
  (error-level service-error-level)
  (binary-path (:string :encoding :utf-16))  ; can include args for auto-start services
  (load-order-group (:string :encoding :utf-16))
  (tag-id (:pointer :ulong)) ; Out, optional
  (dependencies (:string :encoding :utf-16))          ;; double-NUL terminated list of
  ;; NUL-terminated strings
  (service-start-account (:string :encoding :utf-16))
  (password (:string :encoding :utf-16)))

(cffi:defcfun (close-service-handle "CloseServiceHandle"
                                    :library advapi32
                                    :convention :stdcall)
    :int
  (handle sc-handle))

(cffi:define-foreign-library kernel32
  ((or :windows :mswindows :win32) (:default "kernel32")))

(cffi:use-foreign-library kernel32)

(cffi:defcfun (get-last-error "GetLastError"
                              :library kernel32
                              :convention :stdcall)
    :ulong)

(cffi:defcfun (%get-module-name "GetModuleFileNameW"
                                :library kernel32
                                :convention :stdcall)
    :ulong
  (module :pointer)
  (buf (:string :encoding :utf-16))
  (bufsize :ulong))

(defun get-module-name (&optional (module (cffi:null-pointer)))
  (cffi:with-foreign-object (buf :char (* 2 261))
    (%get-module-name module buf (1- (* 2 261)))
    (setf (cffi:mem-aref buf :char 260) 0)
    ;; Why does this have to be :utf-16le instead of just :utf-16?
    (cffi:foreign-string-to-lisp buf :encoding :utf-16le)))

(defun make-deps-string (deps group-deps)
  (with-output-to-string (str)
    (loop for dep in deps
       do (format str "~A~C" dep (code-char 0)))
    (loop for gdep in group-deps
       ;; '+' is SC_GROUP_IDENTIFIER
       do (format str "+~A~C" gdep (code-char 0)))
    ;; Just to be safe (requires a "double null" at the end, which may
    ;; or may not include the null of the last string)
    (write-char (code-char 0) str)))

(defun install-service (name &key (type :win32-shared-process)
                               (start-type :demand-start)
                               (error-level :normal)
                               (load-order-group (cffi:null-pointer))
                               (dependencies '())
                               (group-dependencies '())
                               (account (cffi:null-pointer))
                               (password (cffi:null-pointer))
                               get-tag-id)
  (macrolet ((with-sc-manager ((var) &body body)
               `(let ((,var (open-sc-manager (cffi:null-pointer)
                                             (cffi:null-pointer)
                                             (flags 'sc-manager-rights :all-access))))
                  (when (cffi:null-pointer-p scm-handle)
                    (error "Failed opening the Service Control Manager."))
                  (unwind-protect
                       (progn
                         ,@body)
                    (close-service-handle scm-handle)))))
    (with-sc-manager (scm-handle)
      (cffi:with-foreign-objects ((tag-id :ulong))
        (let ((service-handle (create-service scm-handle
                                              name ;service name
                                              name ; display name
                                              (flags 'sc-manager-rights :all-access)
                                              type
                                              start-type
                                              error-level
                                              (cffi:null-pointer)
                                              load-order-group
                                              (if get-tag-id tag-id (cffi:null-pointer))
                                              (let ((s (make-deps-string dependencies
                                                                         group-dependencies)))
                                                (if (> (length s) 0)
                                                    s
                                                    (cffi:null-pointer)))
                                              account
                                              password)))
          (when (cffi:null-pointer-p service-handle)
            (error "Failed to create the service."))
          (values service-handle (if get-tag-id (cffi:mem-aref tag-id :ulong) nil)))))))

(defclass service ()
  ((status :initarg :status
           :accessor service-status-obj)
   (status-handle :accessor status-handle))
  (:documentation "Wrapper for a windows service."))

(defgeneric tick (service)
  (:documentation "Indicate that another initialization or shutdown step has occurred."))

(defgeneric set-status (service status &optional timeout)
  (:documentation "Set SERVICE's status to STATUS, indicating an error if TIMEOUT has elapsed before the pending operation has completed or a tick occurs."))

(defgeneric handle-control (service control event-type event-data data)
  (:documentation "Control handler for SERVICE."))
