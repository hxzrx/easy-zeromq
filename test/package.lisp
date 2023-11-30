(defpackage #:soft-universal-time-tests
  (:use #:cl #:parachute)
  (:export :test
   :soft-universal-time-tests))


(in-package :soft-universal-time-tests)

(define-test soft-universal-time-tests)
(define-test softime :parent soft-universal-time-tests)
