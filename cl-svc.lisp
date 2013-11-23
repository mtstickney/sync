;;;; cl-svc.lisp

(in-package #:cl-svc)

(cffi:define-foreign-library advapi32
  ((or :mswindows :win32 :windows) (:default "advapi32")))

(cffi:use-foreign-library advapi32)

(cffi:defcstruct service-table-entry
  "SERVICE_TABLE_ENTRY for Windows services."
  (service-name :string)
  (service-func :pointer))

(cffi:defcfun (start-service-ctrl-dispatcher "StartServiceCtrlDispatcherA"
                                             :convention :stdcall
                                             :library advapi32)
    :int
  (service-table (:pointer (:struct service-table))))

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

(cffi:defctype service-status-handle :int "Handle to a SERVICE-STATUS struct.")

(cffi:defcfun (register-service-ctrl-handler "RegisterServiceCtrlHandlerA"
                                             :convention :stdcall
                                             :library advapi32)
    service-status-handle
  "Register a service handler table for use. Returns a handle that can be used to set the service status."
  (service-table (:pointer (:struct service-table-entry))))

(cffi:defcfun (set-status-service "SetServiceStatus"
                                  :convention :stdcall
                                  :library advapi32)
    :int
  (status-handle service-status-handle)
  (status (:pointer (:struct service-status))))
