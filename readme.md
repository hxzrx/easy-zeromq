# easy-zeromq

Wrap of [pzmq](https://github.com/orivej/pzmq) with patterns ready.

## Examples

```commonlisp
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
```

