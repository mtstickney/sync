;;;; rpc-http-proxy.lisp

(in-package #:rpc-http-proxy)

(defvar *config* nil)

(defun snake-case-to-lisp (string)
  (check-type string string)
  (nstring-upcase (substitute #\- #\_ string)))

(defun load-config! ()
  (with-open-file (stream (merge-pathnames #P"config.json" rpc-http-proxy-config:*config-dir*))
    (let ((json:*json-identifier-name-to-lisp* 'snake-case-to-lisp))
      (setf *config* (json:decode-json stream)))))

(defun config (key)
  (check-type key keyword)
  (cdr (assoc key *config*)))

;; We need the server to exit on Ctrl-C, so install a handler when
;; Wookie starts.
(defmethod wookie:start-server :before (listener)
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(defun valid-request-p (request)
  (and (listp request)
       (every (lambda (call)
                (and (assoc :method call)
                     (assoc :service call)))
              request)))

(defun encrypted-connection (host port secret server-keys)
  (check-type host string)
  (check-type port (integer 0))
  (check-type secret string)
  (let ((framer (make-instance 'mtgnet-sys:netstring-framer))
        (transport (make-instance 'mtgnet.async:asynchronous-tcp-transport :address host :port port)))
    (mtgnet.encryption:make-encrypted-connection framer transport secret server-keys)))

(defun rpc-error-result (id error)
  (check-type error mtgnet:remote-error)
  (mtgnet-sys:make-rpc-result :error (mtgnet-sys:make-rpc-error :code (mtgnet-sys:remote-error-code error)
                                                                :message (mtgnet-sys:remote-error-msg error))
                              :id id))

(defun proxy-call (con call)
  (check-type con mtgnet:rpc-connection)
  (check-type call mtgnet-sys:rpc-call)
  (let ((id (mtgnet-sys:rpc-call-id call)))
    (bb:chain (bb:catcher (mtgnet:invoke-rpc-method con
                                                    (mtgnet-sys:rpc-call-service call)
                                                    (mtgnet-sys:rpc-call-method call)
                                                    (mtgnet-sys:rpc-call-args call)
                                                    :notification (null id))
                          (mtgnet:remote-error (c)
                                               c))
      (:attach (result)
               (if (typep result 'mtgnet:remote-error)
                   (rpc-error-result id result)
                   (mtgnet-sys:make-rpc-result :data result
                                               :id id))))))

(defun proxy-request (con request)
  (check-type con mtgnet:rpc-connection)
  (let (response-promise)
    (mtgnet:with-batch-calls (con)
      (setf response-promise (bb:all (map 'list (lambda (call)
                                                  (proxy-call con call)) request))))
    response-promise))

(defun process-request (env)
  (let ((body (getf env :raw-body))
        request)
    (unless (member (getf env :request-method) '(:get :post))
      (return-from process-request `(405 (:allow "GET, POST")
                                         ,(format nil "Invalid HTTP request type ~A."
                                                  (symbol-name (getf env :request-method))))))
    (handler-case
        (setf request (mtgnet-sys:unmarshall-rpc-request body))
      (serious-condition (c)
        (format *debug-io* "Error decoding data: ~A~%." c)
        (return-from process-request '(400 () ("Undecodeable request data.")))))

    ;; Perform the rpc call asynchronously.
    (lambda (responder)
      (let (con)
        (bb:chain (setf con (encrypted-connection (config :server-host)
                                                  (config :server-port)
                                                  (config :secret)
                                                  (config :authorized-keys)))
          (:attach (con)
                   (mtgnet:connect con))
          (:attach ()
                   (proxy-request con request))
          (:attach (results)
                   ;; Disconnect.
                   (mtgnet:disconnect con)
                   ;; Dispose of the secret key (it's not GC-able).
                   (mtgnet.crypto:free-secret (mtgnet.encryption::secret-key con))
                   ;; Construct a response.
                   (funcall responder `(200 () (,(with-output-to-string (json:*json-output*)
                                                   (mtgnet-sys:marshall-rpc-response results))))))
          (:catch (err)
            (mtgnet:disconnect con)
            (mtgnet.crypto:free-secret (mtgnet.encryption::secret-key con))
            (funcall responder `(500 () (,(format nil "Error processing request: ~A." err))))))))))

(defun build-app ()
  ;; Make sure the process doesn't get killed when we try to use
  ;; sodium functions.
  (cr:sodium-init)
  ;; Make sure we have a fresh config.
  (load-config!)
  'process-request)

(defun run-app (argv)
  (clack:clackup (build-app)
                 :server :wookie
                 :port (config :listen-port)
                 :use-thread nil))

(defun main (argv)
  (let ((rpc-http-proxy-config:*config-dir*)))
  )

(defun run-request (service method args)
  (let ((request (with-output-to-string (json:*json-output*)
                   (mtgnet-sys:marshall-rpc-request
                    (list (mtgnet-sys:make-rpc-call :service service
                                                    :method method
                                                    :args args
                                                    :id 32)
                          (mtgnet-sys:make-rpc-call :service service
                                                    :method method
                                                    :args args
                                                    :id 32))))))
    (as:with-event-loop ()
      (with-input-from-string (stream request)
        (let ((res (process-request `(:request-method :get :raw-body ,stream))))
          (if (functionp res)
              (funcall res (lambda (response)
                             (format *debug-io* "Got response: ~S~%" response)
                             (as:exit-event-loop)))
              (format *debug-io* "Got response: ~S~%" res)))))))
