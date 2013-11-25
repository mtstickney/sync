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
