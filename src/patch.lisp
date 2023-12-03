(in-package :pzmq)

(defun curve-keypair-string ()
  "As a supplementation of PZMQ:CURVE-KEYPAIR, this function generates and returns a CurveZMQ keypair.
Return (VALUES PUBLIC-KEY SECRET-KEY), both of type STRING."
  (let ((z85-type '(:array :unsigned-char 41))
        (key-type '(:array :unsigned-char 32))
        (byte '(unsigned-byte 8)))
    (declare (ignore byte))
    (cffi:with-foreign-objects
        ((public-key-z85 z85-type)
         (public-key key-type)
         (secret-key-z85 z85-type)
         (secret-key key-type))
      (with-c-error-check (:int t)
        (pzmq::%curve-keypair public-key-z85 secret-key-z85))
      (with-c-error-check (:pointer t)
        (pzmq::z85-decode public-key public-key-z85))
      (with-c-error-check (:pointer t)
        (pzmq::z85-decode secret-key secret-key-z85))
      #+:ignore
      (values (foreign-array-to-lisp public-key key-type :element-type byte)
              (foreign-array-to-lisp secret-key key-type :element-type byte))
      (values (cffi:foreign-string-to-lisp public-key-z85)
              (cffi:foreign-string-to-lisp secret-key-z85)))))

(export 'curve-keypair-string)
