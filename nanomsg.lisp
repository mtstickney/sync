(in-package :probuild)

(define-condition nanomsg-error (error)
  ((errno :initarg :errno :accessor errno)))

(defun call-with-socket (fun type urls &key (domain nn:+af-sp+) bindp)
  (check-type fun function)
  (check-type type integer)
  (check-type urls list)
  (check-type domain integer)
  (let ((socket (nn:socket domain type))
        (eids '()))
    (when (< socket 0)
      (error 'nanomsg-error :errno (nn:errno)))
    (unwind-protect
         (progn
           (loop for url in urls
              do (let ((eid (if bindp
                                (nn:bind socket url)
                                (nn:connect socket url))))
                   (when (< eid 0)
                     (error 'nanomsg-error :errno (nn:errno)))
                   (push eid eids)))
           (let ((*eids* eids))
             (declare (special *eids*))
             (funcall fun socket)))
      (when (< (nn:close socket) 0)
        (error 'nanomsg-error :errno (nn:errno))))))

(defmacro with-socket ((var type urls &rest keys &key domain bindp) &body body)
  (declare (ignore domain bindp))
  (let ((thunk-var (gensym "THUNK")))
    `(let ((,thunk-var (lambda (,var)
                         (declare (ignorable ,var))
                         ,@body)))
       (call-with-socket ,thunk-var ,type ,urls ,@keys))))

(defun nn-msg ()
  (1- (ash 1 (* 8 (cffi:foreign-type-size :unsigned-int)))))

(defun recv-buffer (s)
  (cffi:with-foreign-object (ptr-ptr :pointer)
    (setf (cffi:mem-aref ptr-ptr :pointer) (cffi:null-pointer))
    (let ((bytes (nn:recv s ptr-ptr (nn-msg) 0)))
      (when (< bytes 0)
        (error 'nanomsg-error :errno (nn:errno)))
      (values (cffi:mem-aref ptr-ptr :pointer)
              bytes))))

(defun msgbuf->ubv (buf len)
  (let ((vec (make-array len :element-type '(unsigned-byte 8))))
           (dotimes (i len)
             (setf (aref vec i)
                   (cffi:mem-aref buf :unsigned-char i)))
           vec))

(defun recv-ubv (s)
  (multiple-value-bind (buf len) (recv-buffer s)
    (unwind-protect
         (msgbuf->ubv buf len)
      (nn:freemsg buf))))

(defun send-buffer (s buf len)
  (let ((bytes (nn:send s buf len 0)))
    (when (< bytes 0)
      (error 'nanomsg-error :errno (nn:errno)))
    bytes))

(defun ubv->msgbuf (ubv &key (type 0))
  (check-type ubv (vector (unsigned-byte 8)))
  (let* ((len (length ubv))
         (msg (nn:allocmsg len type)))
    (when (cffi:null-pointer-p msg)
      (error 'nanomsg-error :errno (nn:errno)))
    (dotimes (i len)
      (setf (cffi:mem-aref msg :unsigned-char i)
            (aref ubv i)))
    msg))

(defun send-ubv (s ubv &key (buf-type 0))
  (check-type ubv (vector (unsigned-byte 8)))
  (let ((buf (ubv->msgbuf ubv :type buf-type)))
    (cffi:with-foreign-object (buf-ptr :pointer)
      (setf (cffi:mem-aref buf-ptr :pointer)
            buf)
      (send-buffer s buf-ptr (nn-msg)))))

(defun rcv-timeout (socket)
  (cffi:with-foreign-objects ((opt :int)
                              (size :unsigned-int))
    (setf (cffi:mem-aref size :unsigned-int)
          (cffi:foreign-type-size :int))
    (let ((ret (nn:getsockopt socket
                              nn:+nn-sol-socket+
                              nn:+nn-rcvtimeo+
                              opt
                              size)))
      (when (< ret 0)
        (error 'nanomsg-error :errno (nn:errno)))
      (cffi:mem-aref opt :int))))

(defun (setf rcv-timeout) (val socket)
  (cffi:with-foreign-object (opt :int)
    (setf (cffi:mem-aref opt :int)
          val)
    (let ((ret (nn:setsockopt socket
                              nn:+nn-sol-socket+
                              nn:+nn-rcvtimeo+
                              opt
                              (cffi:foreign-type-size :int))))
      (when (< ret 0)
        (error 'nanomsg-error :errno (nn:errno))))
    val))

(defun rcv-or-timeout (socket)
  (handler-bind
      ((nanomsg-error (lambda (c)
                        ;; FIXME: nn:+eagain+ is wrong, system EAGAIN
                        ;; is 11
                        (when (=  11 (errno c))
                          (return-from rcv-or-timeout nil)))))
    (recv-ubv socket)))
