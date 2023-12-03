(in-package :easy-zeromq)


(declaim (inline unpack-foreign-msg-to-bytes))
(defun unpack-foreign-msg-to-bytes (msg)
  "Converts a foreign array of unsigned characters to a Lisp vector of such."
  ;; should specify element-type, or else will return a simple-vector which will be failed to decode.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (flet ((copy-foreign-array (pointer len) ; foreign pointer to lisp byte array
           (let ((lisp-vector (make-array len :element-type '(unsigned-byte 8)))) ; also (cffi:make-shareable-byte-vector len)))
             (cffi:with-pointer-to-vector-data (vec-ptr lisp-vector)
               (cffi:foreign-funcall "memcpy" :pointer vec-ptr :pointer pointer :size len :pointer)
               lisp-vector))))
  #+:ignore(cffi:foreign-array-to-lisp (pzmq:msg-data msg) ; this raw rpcq code is very very slow
                              `(:array :uint8 ,(pzmq:msg-size msg))
                              :element-type '(unsigned-byte 8))
  (copy-foreign-array (pzmq:msg-data msg) (pzmq:msg-size msg))))

(defun global-function-p (symbol)
  "Return true if SYMBOL is a symbol naming a global function. Return false otherwise."
  (and (typep symbol 'symbol)
       (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun %port-open-p (port &key (interface "127.0.0.1")) ; lib find-port, MIT
  "Determine if the port is open."
  (handler-case
      (let ((socket (usocket:socket-listen interface port
                                           :reuse-address t)))
        (usocket:socket-close socket))
    (usocket:address-in-use-error (condition)
      (declare (ignore condition))
      nil)))


(defun gen-random-port (&key (host "0.0.0.0"))
  "Return a random open port on host.
Note that the port number returned by this function might be unavailable when using it."
  (let* ((listen (usocket:socket-listen host 0))
         (port (usocket:get-local-port listen)))
    (usocket:socket-close listen)
    port))

(defun gen-listen-address (&optional (protocal :tcp) host port)
  "Generate an listening address of type tcp, ipc, or inproc.
The format of the address is protocal://id1:id2,
and if the protocal is tcp, id1 will be the local ip, and id2 will be the port,
if the protocal is ipc, id1 will be the local ip too, and id2 will an unique number,
if the protocal is inproc, id1 will be the pid, and id2 will be an unique number too."
  (let ((prot (string-upcase (string protocal))))
    (cond ((equal prot "TCP") (format nil "tcp://~d:~d" (if host host "*") (if port port (gen-random-port))))
          ((equal prot "INPROC") (format nil "inproc://~d:~d" (if host host (get-pid)) (if port port (gensym ""))))
          ((equal prot "IPC") (format nil "ipc://~d:~d" (if host host (get-local-ip)) (if port port (gensym ""))))
          (t (error "Unsupported protocal: ~d" protocal)))))

(declaim (inline parse-address))
(defun parse-address (address)
  "Parse an zeromq address to a list of protocal, host, and port."
  ;; (parse-address "tcp://127.0.0.1:5555")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (string address))
  (destructuring-bind (protocal host port) (cl-ppcre:split "(://)|(:)" address)
    (list protocal host port)))

(defun get-pid ()
  "Get the Process ID."
  #+sbcl(sb-posix:getpid)
  #+ccl(ccl::getpid)
  #-(or sbcl ccl) (osicat-posix:getpid))

(defun get-local-ip (&optional (interface-name "eth0"))
  "Get the ip string of localhost whose interface has the name provided."
  (flet ((prompt-new-value (prompt)
           (format *query-io* prompt)
           (force-output *query-io*)
           (list (read *query-io*))))
    (let* ((interfaces (ip-interfaces:get-ip-interfaces))
           (interface (find interface-name interfaces
                            :key #'ip-interfaces:ip-interface-name
                            :test #'string-equal)))
      (restart-case
          (if interface
              (format nil "~{~d~^.~}"
                      (coerce (ip-interfaces:ip-interface-address interface) 'list))
              (error "Cannot find interface"))
        (try-another-interface-name (new-name)
          :report "Choose to provide a new name."
          :interactive (lambda ()
                         (prompt-new-value (format nil "Please enter one interface name of: ~{~d~^, ~}: "
                                                   (loop for interf in interfaces
                                                         collect (ip-interfaces:ip-interface-name interf)))))
          (get-local-ip new-name))))))
