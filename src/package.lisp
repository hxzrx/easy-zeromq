;;;; package.lisp

(defpackage :easy-zeromq
  (:use :cl)
  (:nicknames :ezmq)
  (:export
   :encode
   :decode
   :handler-encode
   :handler-decode
   :defencoding)
  (:export
   :server-auth-config
   :server-auth-config-server-secret-key
   :server-auth-config-server-public-key
   :client-auth-config
   :client-auth-config-client-secret-key
   :client-auth-config-client-public-key
   :client-auth-config-server-public-key
   :make-server-auth
   :make-client-auth
   :gen-server-auth
   :gen-client-auth
   :with-zmq-client
   :start-pull-server
   :start-streamer-pull-server
   :push-string
   :push-bytes
   :push-object
   :with-push-client
   :start-router-dealer-server
   :dealer-send-string
   :dealer-send-bytes
   :dealer-send-object
   :with-dealer-client
   :start-req-rep-server
   :req-send-string
   :req-send-bytes
   :req-send-object
   :with-req-client
   :*default-server-auth*
   :*default-client-auth*))
