;;;; Wrapper of pzmq.

(in-package :easy-zeromq)

(defstruct (server-auth-config (:constructor %make-server-auth (&key server-public-key server-secret-key))
                               (:copier nil)
                               (:predicate server-auth-config-p))
  "Holds the ZeroMQ Curve configuration for a server socket."
  (server-secret-key (error "Must provide SERVER-SECRET-KEY") :read-only t)
  (server-public-key (error "Must provide SERVER-PUBLIC-KEY") :read-only t))

(defstruct (client-auth-config (:constructor %make-client-auth (&key client-public-key client-secret-key server-public-key))
                               (:copier nil)
                               (:predicate client-auth-config-p))
  "Holds the ZeroMQ Curve configuration for a client socket."
  (client-secret-key (error "Must provide CLIENT-SECRET-KEY") :read-only t)
  (client-public-key (error "Must provide CLIENT-PUBLIC-KEY") :read-only t)
  (server-public-key (error "Must provide SERVER-PUBLIC-KEY") :read-only t))

(defun make-server-auth (pub-prv)
  "Return a server-auth-config instance"
  (declare (list pub-prv))
  (check-type (second pub-prv) (or vector string))
  (%make-server-auth :server-public-key (first pub-prv)
                     :server-secret-key (second pub-prv)))

(defun make-client-auth (cli-pub/cli-prv/srv-pub)
  "Return a client-auth-config instance"
  (declare (list cli-pub/cli-prv/srv-pub))
  (check-type (first  cli-pub/cli-prv/srv-pub) (or array string))
  (check-type (second cli-pub/cli-prv/srv-pub) (or array string))
  (check-type (third  cli-pub/cli-prv/srv-pub) (or array string))
  (%make-client-auth :client-public-key (first  cli-pub/cli-prv/srv-pub)
                     :client-secret-key (second cli-pub/cli-prv/srv-pub)
                     :server-public-key (third  cli-pub/cli-prv/srv-pub)))

(defun gen-server-auth (&key (type :string))
  "Generate a CurveZMQ keypair of specified TYPE and the return an instance of server-auth-config.
type is a keyword that can be either :string or :bytes."
  (cond ((eq type :bytes)
         (funcall #'make-server-auth (multiple-value-list (pzmq:curve-keypair))))
        ((eq type :string)
         (funcall #'make-server-auth (multiple-value-list (pzmq:curve-keypair-string))))
        (t (error "Unsupported type of CurveZMQ keypair: ~d" type))))

(defun gen-client-auth (server-pub-key &key (type :string)) ; (gen-client-auth (pzmq:curve-keypair))
  "Generate a CurveZMQ keypair of specified TYPE and the return an instance of client-auth-config.
type is a keyword that can be either :string or :bytes."
  (cond ((eq type :bytes)
         (funcall #'make-client-auth
                  (append (multiple-value-list (pzmq:curve-keypair))
                          (list server-pub-key))))
        ((eq type :string)
         (funcall #'make-client-auth
                  (append (multiple-value-list (pzmq:curve-keypair-string))
                          (list server-pub-key))))
        (t (error "Unsupported type of CurveZMQ keypair: ~d" type))))


(defvar *default-server-auth* (make-server-auth (list ">sYo=kjLRlpINu@N#ZXMlJ6Chbpo7YUqj.<fFDKR"
                                                      "]APCC{}H{De<Gw}uJZHpGn7JeI-+Cm.5M1no.j1#"))
  "A zeromq curve (public secret) keypair strings for server useage. DO NOT use it in product.")

(defvar *default-client-auth* (make-client-auth (list "/u09FuJCyw4R:*%a9%3y<)eXwu%sKzh8iTP18k=2"
                                                      "sgn^1Y+mQKDT-5QR<nBnpyS8xqolFWwad%N=-(r4"
                                                      ">sYo=kjLRlpINu@N#ZXMlJ6Chbpo7YUqj.<fFDKR"))
  "A zeromq curve (server-public client-public client-secret) keypair strings for client usage. DO NOT use it in product.")


;;; simple wrap to pzmq:send

(defun %%zmq-send-string (socket msg-string)
  "Send a lisp string to a zeromq socket."
  (pzmq:send socket msg-string))

(defun %%zmq-send-bytes (socket msg-bytes)
  "Send a lisp byte vector to a zeromq socket."
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare ((simple-array (unsigned-byte 8) (*)) msg-bytes))
  (cffi:with-pointer-to-vector-data (msg-ptr msg-bytes)
    (pzmq:send socket msg-ptr :len (the fixnum (length msg-bytes)))))

(defun %%zmq-send-object (socket msg-object)
  "Send a lisp object to a zeromq socket, the object will be serializd to a byte vector first."
  (alexandria:if-let (bytes (handler-encode msg-object))
    (handler-case
        (%%zmq-send-bytes socket bytes)
      (error (e)
        (log:error "Send object ~d of bytes ~d raised an error ~d" msg-object bytes e)))
    (log:debug "Message pushing failed due to encoding failure: ~d" msg-object)))


(defmacro with-zmq-client ((socket endpoint type auth) &body body)
  "Connect to the server which is described by ENDPOINT and exec the BODY,
lisp messages can be sent by %%zmq-send-* or pzmq:send,
SOCKET is the symbol which will bind to the client socket and it can be used within BODY.
type: can be any valid client type such as :push, :req, :dealer"
  (let (($auth (gensym)))
    `(let ((,$auth ,auth))
       (pzmq:with-socket ,socket ,type
         (when ,$auth
           (pzmq:setsockopt ,socket :curve-secretkey (client-auth-config-client-secret-key ,$auth))
           (pzmq:setsockopt ,socket :curve-publickey (client-auth-config-client-public-key ,$auth))
           (pzmq:setsockopt ,socket :curve-serverkey (client-auth-config-server-public-key ,$auth)))
         (pzmq:connect ,socket ,endpoint)
         ,@body))))


;;; --- pull/push ---

;; pull server
(defun %start-pull-server (msg-handler endpoints auth-config)
  "Receive a :push messages from its listening endpoints.
msg-handler is a function which accept one argument of lisp byte array.
endpoints: an endpoint string or a list of endpoint strings.
auth-config: type of server-auth-config or nil, "
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function msg-handler))
  (pzmq:with-sockets ((receiver :pull))
    (when auth-config
      (pzmq:setsockopt receiver :curve-server t)
      (pzmq:setsockopt receiver :curve-secretkey (server-auth-config-server-secret-key auth-config)))
    (if (listp endpoints)
        (dolist (addr endpoints) ; binding to multiple addrs should be tested
          (log:info "Start pull server listening on ~d" addr)
          (pzmq:bind receiver addr))
        (progn (log:info "Start pull server listening on ~d" endpoints)
               (pzmq:bind receiver endpoints)))
    (loop (pzmq:with-message msg
            (pzmq:msg-recv msg receiver)
            (handler-case (funcall msg-handler (unpack-foreign-msg-to-bytes msg))
              (error (e)
                (log:error "Handling message <~d> raised an error <~d> in pull server"
                           (unpack-foreign-msg-to-bytes msg) e)))))))

(defun start-pull-server (msg-handler &optional (endpoints "tcp://*:5555") auth-config bindings)
  "Create a new thread and run a pull server."
  #|
  (nactor::start-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)))
  (nactor::start-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)) "ipc://xx")
  (nactor::start-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)) "inproc://xxx")

  (nactor::push-string "tcp://127.0.0.1:5555" "abcd" nil)
  (nactor::push-bytes "tcp://127.0.0.1:5555" (nactor::encode "abcd") nil)

  (nactor::push-string "ipc://xx" "abcd" nil)
  (nactor::push-bytes "ipc://xx" (nactor::encode "abcd") nil)

  (nactor::push-string "inproc://xxx" "abcd" nil)
  (nactor::push-bytes "inproc://xxx" (nactor::encode "abcd") nil)
  |#
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (log:debug "bindings in start-pull-server: ~d" bindings)
  (pzmq:with-context nil ; this is critical if the protocal is inproc
    (if bindings
        (bt:make-thread (lambda ()
                          (progv (mapcar #'car bindings)
                              (loop for (nil form) in bindings collect (eval form))
                            (unwind-protect (%start-pull-server msg-handler endpoints auth-config)
                              (log:error "Pull server ~d was exit unintentionally." (bt:thread-name (bt:current-thread)))
                              (ignore-errors (bt:destroy-thread (bt:current-thread))))))
                        :name (string (gensym "PULL-SERVER-")))
        (bt:make-thread (lambda ()
                          (unwind-protect (%start-pull-server msg-handler endpoints auth-config)
                            (log:error "Pull server ~d was exit unintentionally." (bt:thread-name (bt:current-thread)))
                            (ignore-errors (bt:destroy-thread (bt:current-thread)))))
                        :name (string (gensym "PULL-SERVER-"))))))

(defun %start-streamer-pull-server (msg-handler listen-addresses listener-count auth-config bindings)
  (let ((pool-addr (format nil "inproc://~d" (uuid:make-v4-uuid)))
        (server nil))
    (pzmq:with-sockets ((frontend :pull) (backend :push))
      (when auth-config
        (pzmq:setsockopt frontend :curve-server t)
        (pzmq:setsockopt frontend :curve-secretkey (server-auth-config-server-secret-key auth-config)))
      (if (listp listen-addresses)
          (dolist (addr listen-addresses) ; binding to multiple addrs should be tested
            (log:info "Start streamer pull server listening on ~d" addr)
            (pzmq:bind frontend addr))
          (progn (log:info "Start streamer pull server listening on ~d" listen-addresses)
                 (pzmq:bind frontend listen-addresses)))
      (pzmq:bind backend pool-addr)
      (unwind-protect
           (progn
             (dotimes (i listener-count)
               (pzmq:with-context nil
                 (push (if bindings
                           (bt:make-thread (lambda ()
                                             (progv (mapcar #'car bindings)
                                                 (loop for (nil form) in bindings collect (eval form))
                                               (pzmq:with-socket receiver :pull
                                                 (pzmq:connect receiver pool-addr)
                                                 (loop (pzmq:with-message msg
                                                         (pzmq:msg-recv msg receiver)
                                                         ;;(log:debug "~&debug: recv new msg: ~d~%" msg)
                                                         (funcall msg-handler (unpack-foreign-msg-to-bytes msg)))))))
                                           :name (format nil "STREAMER/PULL-WORKER-~d" i))
                           (bt:make-thread (lambda ()
                                             (pzmq:with-socket receiver :pull
                                               (pzmq:connect receiver pool-addr)
                                               (loop (pzmq:with-message msg
                                                       (pzmq:msg-recv msg receiver)
                                                       ;;(log:debug "~&debug: recv new msg: ~d~%" msg)
                                                       (funcall msg-handler (unpack-foreign-msg-to-bytes msg))))))
                                           :name (format nil "STREAMER/PULL-WORKER-~d" i)))
                       server)))
             (pzmq:device :streamer frontend backend))
        (mapc #'bt:destroy-thread server)))))

(defun start-streamer-pull-server (msg-handler &optional (listen-addresses "tcp://*:5556") (listener-count 2) auth-config bindings)
  "Create a new thread and run a streamer pull server."
  #|
  (nactor::start-streamer-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)))
  (nactor::start-streamer-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)) "ipc://yy")
  (nactor::start-streamer-pull-server (lambda (x) (format t "~&recv a msg: ~d~%" x)) "inproc://yyy")

  (nactor::push-string "tcp://127.0.0.1:5556" "abcd" nil)
  (nactor::push-bytes "tcp://127.0.0.1:5556" (nactor::encode "abcd") nil)

  (nactor::push-string "ipc://yy" "abcd" nil)
  (nactor::push-bytes "ipc://yy" (nactor::encode "abcd") nil)

  (nactor::push-string "inproc://yyy" "abcd" nil)
  (nactor::push-bytes "inproc://yyy" (nactor::encode "abcd") nil)
  |#
  (bt:make-thread (lambda ()
                    (unwind-protect (%start-streamer-pull-server msg-handler listen-addresses listener-count auth-config bindings)
                      (log:error "Streamer pull server ~d was exit unintentionally." (bt:thread-name (bt:current-thread)))
                      (ignore-errors (bt:destroy-thread (bt:current-thread)))))
                  :name (string (gensym "STREAMER-PULL-SERVER-"))))

;; push client
;; push client for string msgs

#+:ignore
(defun %%zmq-push-string (socket msg-string)
  "Send a lisp string to a zeromq socket."
  (pzmq:send socket msg-string))

(defun %zmq-push-string (destination msg-string auth)
  "Send a :pull message of lisp string to :push server with the destination endpoint.
msg-string: a string type of message.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (pzmq:with-sockets ((sender :push))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-string sender msg-string)))

;; push client for binary msgs
;;(declaim (inline %%zmq-push-bytes))
#+:ignore
(defun %%zmq-push-bytes (socket msg-bytes)
  "Send a lisp byte vector to a zeromq socket."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare ((simple-array (unsigned-byte 8) (*)) msg-bytes))
  (cffi:with-pointer-to-vector-data (msg-ptr msg-bytes)
    (pzmq:send socket msg-ptr :len (the fixnum (length msg-bytes)))))

(defun %zmq-push-bytes (destination msg-bytes auth)
  "Send a :pull message of lisp byte vector to :push server with the destination endpoint.
msg-bytes: a lisp simple-array of type (unsigned-byte 8), it may be a decoded result by conspack.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :push))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-bytes sender msg-bytes)))

;; push client for lisp objects
#+:ignore
(defun %%zmq-push-object (socket msg-object)
  "Send a lisp object to a zeromq socket, the object will be serializd to a byte vector first."
  (alexandria:if-let (bytes (handler-encode msg-object))
    (%%zmq-push-bytes socket bytes)
    (log:debug "Message pushing failed due to encoding failure: ~d" msg-object)))

(defun %zmq-push-object (destination msg-object auth)
  "Send a :pull message of lisp object to :push server with the destination endpoint.
msg-object: a lisp object, will be serialized to a byte vetor.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :push))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-object sender msg-object)))

(defun push-string (destination str auth)
  "Send a string to a pull server."
  (%zmq-push-string destination str auth))

(defun push-bytes (destination bytes auth)
  "Send a bytes array to a pull server."
  (%zmq-push-bytes destination bytes auth))

(defun push-object (destination object auth)
  "Send a bytes array to a pull server."
  (%zmq-push-object destination object auth))

#+:ignore
;; this function may be confusing
(defun zmq-push-message (msg dest auth)
  "Send an any type of lisp object to a pull server"
  (typecase msg
    ((simple-array (unsigned-byte 8) (*))
     (zmq-push-bytes dest msg auth))
    (string
     (zmq-push-string dest msg auth))
    (t (zmq-push-object dest msg auth))))

;; Note that the function above will create a new connection for each msg, it's wasty.

(defmacro with-push-client ((sender endpoint &optional auth) &body body)
  "Connect to the server which is described by ENDPOINT and exec the BODY,
lisp messages can be sent by %%zmq-send-* or pzmq:send,
SENDER is the symbol which will bind to the client socket and it can be used within BODY."
  `(with-zmq-client (,sender ,endpoint :push ,auth)
     ,@body))


;;; --- router/dealer ---

;; server part
(defun %dealer-receive-raw-message (recv-socket)
  "Pulls a ZMQ request over the RECEIVER socket.  Returns a VALUES triple:

* IDENTITY-FRAME: array of (UNSIGNED-BYTE 8) that describes the intended recipient of a reply to this request.
* NULL-FRAME?:    boolean indicating whether the recipient expects an additional null frame after the identity frame. (This is the case for REQ-type clients.)
* PAYLOAD:        array of (UNSIGNED-BYTE 8) that houses the raw request."
  ;; ZeroMQ requests come in one of two flavors:
  ;;
  ;;   (1) identity frame, null frame, data frame
  ;;   (2) identity frame, data frame
  ;;
  ;; whichever we get, we also have to reply in the same way, so we track the
  ;; format in addition to the data.
  (pzmq:with-message msg
    (pzmq:msg-recv msg recv-socket) ; recv identity frame
    (let ((identity (unpack-foreign-msg-to-bytes msg)))
      (assert (pzmq:getsockopt recv-socket :rcvmore))
      (pzmq:with-message msg
        (pzmq:msg-recv msg recv-socket) ; recv the 2nd frame
        (cond
          ((pzmq:getsockopt recv-socket :rcvmore) ; if there's one more frame
           (assert (zerop (pzmq:msg-size msg))) ; it should be an empty frame
           (pzmq:with-message msg
             (pzmq:msg-recv msg recv-socket) ; recv the 3rd frame
             (assert (not (pzmq:getsockopt recv-socket :rcvmore))) ; there should be no more frames
             (values
              identity
              t
              (unpack-foreign-msg-to-bytes msg))))
          (t
           (values
            identity
            nil
            (unpack-foreign-msg-to-bytes msg))))))))

(defun %dealer-process-raw-message (msg-handler recv-socket)
  "Read a raw request from RECEIVER socket.
The decoded request message will be sent to the actor to get further processing."
  (handler-bind ((error (lambda (c)
                          ;; we can't even reply to the client. log the error and return.
                          (log:error "Threw generic error before actor sending: ~a" c)
                          (return-from %dealer-process-raw-message))))
    (multiple-value-bind (identity empty-frame lisp-msg-bytes) (%dealer-receive-raw-message recv-socket)
      (log:debug "Received a message ~d" lisp-msg-bytes)
      (funcall msg-handler recv-socket lisp-msg-bytes identity empty-frame))))

(defun %dealer-thread-worker (msg-handler pool-address)
  "The thread body for dealer to receive and process messages."
  (pzmq:with-socket recv-socket :dealer
    (pzmq:connect recv-socket pool-address)
    (loop (%dealer-process-raw-message msg-handler recv-socket))))

(defun %dealer-reply-bytes (socket identity null-frame? payload)
  "Pushes a ZMQ reply onto SOCKET.  Takes the following values:

* IDENTITY:    array of (UNSIGNED-BYTE 8) that describes the inteded recipient of the reply.  Copy this from the matching %PULL-RAW-REQUEST.
* NULL-FRAME?: boolean indicating whether the recipient expects an additional null frame after the identity frame. Copy this from the matching %PULL-RAW-REQUEST.
* PAYLOAD:     array of (UNSIGNED-BYTE 8) that houses the raw reply."
  ;; this function may be used by the msg-handler to reply the message
  (cffi:with-pointer-to-vector-data (id-ptr identity)
    (pzmq:send socket id-ptr :len (length identity) :sndmore t))
  (when null-frame?
    (pzmq:send socket "" :len 0 :sndmore t))
  (cffi:with-pointer-to-vector-data (pl-ptr payload)
    (pzmq:send socket pl-ptr :len (length payload) :sndmore nil))
  t)

;; the reply function below are useful for msg-handler of router-dealer-server

(defun dealer-reply-string (socket identity null-frame? lisp-string)
  (%dealer-reply-bytes socket identity null-frame? (babel:string-to-octets lisp-string))) ; babel may be faster than flexi-streams

(defun dealer-reply-bytes (socket identity null-frame? lisp-bytes-vector)
  (%dealer-reply-bytes socket identity null-frame? lisp-bytes-vector))

(defun dealer-reply-object (socket identity null-frame? lisp-object)
  (%dealer-reply-bytes socket identity null-frame? (encode lisp-object)))

(defun %start-router-dealer-server (msg-handler listen-addresses listener-count auth-config bindings)
  "Main loop of an RPCQ server.
Argument descriptions:
 * MSG-HANDLER is a function accepts 4 args: recv-socket lisp-msg-bytes identity empty-frame
 * AUTH-CONFIG is the SERVER-AUTH-CONFIG specifying keys for socket encryption.
 * LISTEN-ADDRESSES is a list of strings, each of which is a valid ZMQ interface address that the server will listen on.
 * THREAD-COUNT is a positive integer of the number of worker threads that the server will spawn to service requests.
"
  (let ((server)
        (pool-address (format nil "inproc://~a" (uuid:make-v4-uuid))))
    (log:info "Spawning network listener server at ~a .~%" listen-addresses)
    (pzmq:with-sockets ((clients :router) (workers :dealer))
      ;; Configuring the server secret key here enables encryption on the socket and allows clients
      ;; to authenticate the server. However, this server is not currently authenticating connected
      ;; client keys. In order to do, we'd need to implement that authentication ourselves on top of
      ;; the ZeroMQ Authentication Protocol (ZAP) (unless such support is added to PZMQ).
      ;;
      ;; https://rfc.zeromq.org/spec/27/
      (when auth-config
        (pzmq:setsockopt clients :curve-server t)
        (pzmq:setsockopt clients :curve-secretkey (server-auth-config-server-secret-key auth-config)))
      (handler-bind ((error (lambda (c)
                              (log:error "Address binding failure: ~a, failed to start a router/dealer server" c)
                              (return-from %start-router-dealer-server))))
        #+:ignore
        (dolist (address listen-addresses)
          (pzmq:bind clients address))
        (if (listp listen-addresses)
            (dolist (addr listen-addresses) ; binding to multiple addrs should be tested
              (log:info "Start router/dealer server listening on ~d" addr)
              (pzmq:bind clients addr))
            (progn (log:info "Start router/dealer server listening on ~d" listen-addresses)
                   (pzmq:bind clients listen-addresses))))
      (pzmq:bind workers pool-address)
      (unwind-protect
           (progn
             (dotimes (j listener-count)
               (pzmq:with-context nil
                 (push (if bindings
                           (bt:make-thread (lambda ()
                                             (progv (mapcar #'car bindings)
                                                 (loop for (nil form) in bindings collect (eval form))
                                               (%dealer-thread-worker msg-handler pool-address)))
                                           :name (format nil "ROUTER/DEALER-WORKER-~d" j))
                           (bt:make-thread (lambda () (%dealer-thread-worker msg-handler pool-address))
                                       :name (format nil "ROUTER/DEALER-WORKER-~d" j)))
                       server)))
             ;; http://api.zeromq.org/2-1:zmq-device
             ;; The device connects a frontend socket to a backend socket.
             (pzmq:device :queue clients workers))
        (mapc #'bt:destroy-thread server)))))

(defun start-router-dealer-server (msg-handler &optional (listen-addresses "tcp://*:5557") (listener-count 2) auth-config bindings)
  "Create a new thread and run a router/dealer server.
Argument descriptions:
 * MSG-HANDLER is a function accepts 4 args: recv-socket lisp-msg-bytes identity empty-frame
 * AUTH-CONFIG is the SERVER-AUTH-CONFIG specifying keys for socket encryption.
 * LISTEN-ADDRESSES is string or a list of strings, each of which is a valid ZMQ interface address that the server will listen on.
 * THREAD-COUNT is a positive integer of the number of worker threads that the server will spawn to service requests."
  #|
  (nactor::start-router-dealer-server (lambda (socket bytes id emp) (format t "~&recv a msg: ~d~%" bytes)))
  (nactor::start-router-dealer-server (lambda (socket bytes id emp) (format t "~&recv a msg: ~d~%" bytes)) "ipc://zz")
  (nactor::start-router-dealer-server (lambda (socket bytes id emp) (format t "~&recv a msg: ~d~%" bytes)) "inproc://zzz")

  (nactor::dealer-send-string "tcp://127.0.0.1:5557" "abcd" nil)
  (nactor::dealer-send-bytes "tcp://127.0.0.1:5557" (nactor::encode "abcd") nil)

  (nactor::dealer-send-string "ipc://zz" "abcd" nil)
  (nactor::dealer-send-bytes "ipc://zz" (nactor::encode "abcd") nil)

  (nactor::dealer-send-string "inproc://zzz" "abcd" nil)
  (nactor::dealer-send-bytes "inproc://zzz" (nactor::encode "abcd") nil)
  |#
  (bt:make-thread (lambda ()
                    (unwind-protect (%start-router-dealer-server msg-handler listen-addresses listener-count auth-config bindings)
                      (log:error "Router dealer server ~d was exit unintentionally." (bt:thread-name (bt:current-thread)))
                      (ignore-errors (bt:destroy-thread (bt:current-thread)))))
                  :name (string (gensym "ROUTER/DEALER-SERVER-"))))

;; router/dealer client part

(defun %dealer-send-string (destination msg-string reply-handler auth dontwait)
  "Send a :dealer message of lisp string to router/dealer server with the destination endpoint.
msg-string: a string type of message.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (pzmq:with-sockets ((sender :dealer))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-string sender msg-string)
    (when reply-handler
      (pzmq:with-message msg
        (pzmq:msg-recv msg sender :dontwait dontwait)
        (funcall reply-handler (unpack-foreign-msg-to-bytes msg))))))

(defun %dealer-send-bytes (destination msg-bytes reply-handler auth dontwait)
  "Send a :dealer message of lisp byte vector to :router/dealer server with the destination endpoint.
msg-bytes: a lisp simple-array of type (unsigned-byte 8), it may be a decoded result by conspack.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :dealer))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-bytes sender msg-bytes)
    (when reply-handler
      (pzmq:with-message msg
        (pzmq:msg-recv msg sender :dontwait dontwait)
        (funcall reply-handler (unpack-foreign-msg-to-bytes msg))))))

(defun %dealer-send-object (destination msg-object reply-handler auth dontwait)
  "Send a :dealer message of lisp object to :router/dealer server with the destination endpoint.
msg-object: a lisp object, will be serialized to a byte vetor.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :dealer))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-object sender msg-object)
    (when reply-handler
      (pzmq:with-message msg
        (pzmq:msg-recv msg sender :dontwait dontwait)
        (funcall reply-handler (unpack-foreign-msg-to-bytes msg))))))

(defun dealer-send-string (destination str &optional reply-handler auth dontwait)
  "Send a string to a router/dealer server.
reply-handler: a function that accept the replied lisp message of bytes array."
  (%dealer-send-string destination str reply-handler auth dontwait))

(defun dealer-send-bytes (destination bytes &optional reply-handler auth dontwait)
  "Send a bytes array to a router/dealer server.
reply-handler: a function that accept the replied lisp message of bytes array."
  (%dealer-send-bytes destination bytes reply-handler auth dontwait))

(defun dealer-send-object (destination object &optional reply-handler auth dontwait)
  "Send a bytes array to a router/dealer server.
reply-handler: a function that accept the replied lisp message of bytes array."
  (%dealer-send-object destination object reply-handler auth dontwait))

(defmacro with-dealer-client ((sender endpoint &optional auth) &body body)
  "Connect to the server which is described by ENDPOINT and exec the BODY,
lisp messages can be sent by %%zmq-send-* or pzmq:send
SENDER is the symbol which will bind to the client socket and it can be used within BODY."
  `(with-zmq-client (,sender ,endpoint :dealer ,auth)
     ,@body))


;;; --- req/rep ---

(defun %zmq-rep-server (msg-handler &optional (endpoints "tcp://*:5558") auth-config)
  "Receive a :push messages from a list of endpoints"
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function msg-handler))
  (pzmq:with-sockets ((receiver :rep))
    (pzmq:with-context nil
      (when auth-config
        (pzmq:setsockopt receiver :curve-server t)
        (pzmq:setsockopt receiver :curve-secretkey (server-auth-config-server-secret-key auth-config)))
      (if (listp endpoints)
          (dolist (addr endpoints) ; binding to multiple addrs should be tested
            (log:info "Start req/rep server listening on ~d" addr)
            (pzmq:bind receiver addr))
          (progn (log:info "Start req/rep server listening on ~d" endpoints)
                 (pzmq:bind receiver endpoints)))
      (loop (pzmq:with-message msg
              (pzmq:msg-recv msg receiver)
              (log:trace "Rep server received a message: ~d" (unpack-foreign-msg-to-bytes msg))
              (pzmq:send receiver (funcall msg-handler (unpack-foreign-msg-to-bytes msg))))))))

(defun start-req-rep-server (msg-handler &optional (endpoints "tcp://*:5558") auth-config bindings)
  "Create a new thread and run a req/rep server.
msg-handler: a function that accepts only one argument, the serialized byte vector of the message."
  #|
  (nactor::start-req-rep-server (lambda (bytes) (format t "~&recv a msg: ~d~%" bytes) bytes))
  (nactor::start-req-rep-server (lambda (bytes) (format t "~&recv a msg: ~d~%" bytes) bytes) "ipc://vv")
  (nactor::start-req-rep-server (lambda (bytes) (format t "~&recv a msg: ~d~%" bytes) bytes) "inproc://vvv")

  (format t "~&tcp reply: ~d~%" (nactor::req-send-string "tcp://127.0.0.1:5558" "abcd" nil))
  (format t "~&tcp reply: ~d~%" (nactor::req-send-bytes "tcp://127.0.0.1:5558" (nactor::encode "abcd") nil))

  (format t "~&ipc reply: ~d~%" (nactor::req-send-string "ipc://vv" "abcd" nil))
  (format t "~&ipc reply: ~d~%" (nactor::req-send-bytes "ipc://vv" (nactor::encode "abcd") nil))

  (format t "~&inproc reply: ~d~%" (nactor::req-send-string "inproc://vvv" "abcd" nil))
  (format t "~&inproc reply: ~d~%" (nactor::req-send-bytes "inproc://vvv" (nactor::encode "abcd") nil))
  |#
  (bt:make-thread (lambda ()
                    (progv (mapcar #'car bindings)
                        (loop for (nil form) in bindings collect (eval form))
                      (%zmq-rep-server msg-handler endpoints auth-config)))
                  :name (string (gensym "REQ/REP-SERVER-"))))

;; reply client

(defun %req-send-string (destination msg-string auth dontwait)
  "Send a :req message of lisp string to :rep server with the destination endpoint.
msg-string: a string type of message.
reply-handler: a function that accept the replied lisp message of bytes array.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (pzmq:with-sockets ((sender :req))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-string sender msg-string)
    (pzmq:with-message msg
      (pzmq:msg-recv msg sender :dontwait dontwait)
      (unpack-foreign-msg-to-bytes msg))))

(defun %req-send-bytes (destination msg-bytes auth dontwait)
  "Send a :req message of lisp byte vector to :rep server with the destination endpoint.
msg-bytes: a lisp simple-array of type (unsigned-byte 8), it may be a decoded result by conspack.
reply-handler: a function that accept the replied lisp message of bytes array.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :req))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-bytes sender msg-bytes)
    (pzmq:with-message msg
      (pzmq:msg-recv msg sender :dontwait dontwait)
      (unpack-foreign-msg-to-bytes msg))))

(defun %req-send-object (destination msg-object auth dontwait)
  "Send a :req message of lisp object to :rep server with the destination endpoint.
msg-object: a lisp object, will be serialized to a byte vetor.
reply-handler: a function that accept the replied lisp message of bytes array.
destination: a valid endpoint this msg will be sent, the format is a string like tcp://127.0.0.1:5555"
  (pzmq:with-sockets ((sender :req))
    ;;(pzmq:with-context nil ; this line can be removed, but cannot be ahead of with-sockets
    (when auth
      (pzmq:setsockopt sender :curve-secretkey (client-auth-config-client-secret-key auth))
      (pzmq:setsockopt sender :curve-publickey (client-auth-config-client-public-key auth))
      (pzmq:setsockopt sender :curve-serverkey (client-auth-config-server-public-key auth)))
    (pzmq:connect sender destination)
    (%%zmq-send-object sender msg-object)
    (pzmq:with-message msg
      (pzmq:msg-recv msg sender :dontwait dontwait)
      (unpack-foreign-msg-to-bytes msg))))

(defun req-send-string (destination str &optional reply-handler auth dontwait)
  "Send a string to a rep server. Return the bytes that the server replied.
reply-handler: a function that accept the replied lisp message of bytes array."
  (if reply-handler
      (funcall reply-handler (%req-send-string destination str auth dontwait))
      (%req-send-string destination str auth dontwait)))

(defun req-send-bytes (destination bytes &optional reply-handler auth dontwait)
  "Send a bytes array to a rep server. Return the bytes that the server replied.
reply-handler: a function that accept the replied lisp message of bytes array."
  (if reply-handler
      (funcall reply-handler (%req-send-bytes destination bytes auth dontwait))
      (%req-send-bytes destination bytes auth dontwait)))

(defun req-send-object (destination object &optional reply-handler auth dontwait)
  "Send a bytes array to a rep server. Return the bytes that the server replied.
reply-handler: a function that accept the replied lisp message of bytes array."
  (if reply-handler
      (funcall reply-handler (%req-send-object destination object auth dontwait))
      (%req-send-object destination object auth dontwait)))

(defmacro with-req-client ((sender endpoint &optional auth) &body body)
  "Connect to the server which is described by ENDPOINT and exec the BODY,
lisp messages can be sent by %dealer-send-message,
SENDER is the symbol which will bind to the client socket and it can be used within BODY."
  `(with-zmq-client (,sender ,endpoint :req ,auth)
     ,@body))


#|
;; request reply example
(defun %string-msg-handler (msg)
  (format t "~&Server recv a msg: ~d~%" (babel:octets-to-string msg))
  (let ((rep "World"))
    (format t "~&Server replies with: ~d~%" rep)
    (encode rep)))

(start-req-rep-server #'%string-msg-handler "tcp://*:5558")
(with-zmq-req-client (sender "tcp://127.0.0.1:5558")
  (format t "~&Client send a request: ~d" "Hello")
  (pzmq:send sender (babel:string-to-octets "Hello"))
  (pzmq:with-message msg
    (pzmq:msg-recv msg sender)
    (format t "~&Client recved a reply: ~d" (decode (unpack-foreign-msg-to-bytes msg)))))

(start-req/rep-server #'%string-msg-handler "inproc://workers")
(with-zmq-req-client (sender "inproc://workers")
  (format t "~&Client send a request: ~d" "Hello")
  (pzmq:send sender (babel:string-to-octets "Hello"))
  (pzmq:with-message msg
    (pzmq:msg-recv msg sender)
    (format t "~&Client recved a reply: ~d" (decode (unpack-foreign-msg-to-bytes msg)))))
|#
