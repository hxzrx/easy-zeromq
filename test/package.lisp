(defpackage #:easy-zeromq-tests
  (:use #:cl #:parachute)
  (:export :test
   :easy-zeromq-tests))


(in-package :easy-zeromq-tests)

(define-test easy-zeromq-tests)
(define-test encoding :parent easy-zeromq-tests)
(define-test zeromq :parent easy-zeromq-tests)
