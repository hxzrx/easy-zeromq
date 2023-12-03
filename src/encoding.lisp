;;;; encoding/decoding an object utilizing cl-conspack.
;;;;
;;;; conspack will encode/decode correctly for inner types,
;;;; however, the class types struct types condition types should define encodings first,
;;;; by invoking conspack:defencoding.
;;;;
;;;; The encoded byte vectors can be sent via the network.
;;;;
;;;; When one forgets to make a defencoding to a class,
;;;; even if the defencoding can be done in a restarter
;;;; where the slots can be listed by meta object protocal and defencoding then.
;;;; But that will not work for structs, and decoding will most likely be another issue.
;;;; So it'a a good practice to make the defencoding in a seperate file.

#|
(defclass user ()
  ((name :initarg :name :initform "" :accessor user-name)))

(conspack:defencoding user name)

(equal "xx" (user-name (decode (encode (make-instance 'user :name "xx")))))

|#


(in-package :easy-zeromq)

(declaim (inline encode))
(defun encode (object)
  "Encode an lisp object and return a byte vector."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (conspack:encode object))

(declaim (inline decode))
(defun decode (byte-vector)
  "Decode a byte-vector and return a lisp object."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (conspack:decode byte-vector))

(declaim (inline handler-encode))
(defun handler-encode (object)
  "Will return nil if OBJECT cannot be encoded.
Such OBJECT may be a class/struct instance which is not defined by defencoding."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (handler-case (encode object)
    (error (e)
      (log:error "Encoding ~d raised an error ~d" object e))))

(declaim (inline handler-decode))
(defun handler-decode (byte-vector)
  "Will return nil if BYTE-VECTOR cannot be decoded."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (handler-case (decode byte-vector)
    (error (e)
      (log:error "Decoding ~d raised an error ~d" byte-vector e))))

(defmacro defencoding (class-name &body slot-names)
  "Simple wrap over conspack:defencoding.
eg. (defencoding class-name slot-a slot-b)"
  `(conspack:defencoding ,class-name ,@slot-names))
