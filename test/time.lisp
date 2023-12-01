(in-package :soft-universal-time-tests)


(define-test get-time-policy :parent softime
  (is eq (soft-universal-time::get-time-policy) soft-universal-time::*time-policy*))

(define-test set-time-policy :parent softime
  (finish (soft-universal-time::set-time-policy :hard))
  (is eq (soft-universal-time::get-time-policy) :hard)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-hard-time)

  (finish (soft-universal-time::set-time-policy :soft))
  (is eq (soft-universal-time::get-time-policy) :soft)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-soft-time)

  (fail (soft-universal-time::set-time-policy :xx)))

(define-test get-soft-time-resolution :parent softime
  (is equal (soft-universal-time::get-soft-time-resolution) soft-universal-time::*soft-time-resolution*))

(define-test set-soft-time-resolution :parent softime
  (let ((res1 0.6)
        (res2 2/3)
        (res3 3))
    (finish (soft-universal-time::set-soft-time-resolution res1))
    (is = (soft-universal-time::get-soft-time-resolution) (coerce res1 'single-float))

    (finish (soft-universal-time::set-soft-time-resolution res2))
    (is = (soft-universal-time::get-soft-time-resolution) (coerce res2 'single-float))

    (finish (soft-universal-time::set-soft-time-resolution res3))
    (is = (soft-universal-time::get-soft-time-resolution) (coerce res3 'single-float))))

(define-test get-soft-time :parent softime
  (is = (soft-universal-time::get-soft-time) soft-universal-time::**soft-universal-time**))

(define-test set-soft-time :parent softime
  (finish (soft-universal-time::set-soft-time 0))
  (is = (soft-universal-time::get-soft-time) 0)

  (finish (soft-universal-time::set-soft-time 123456789))
  (is = (soft-universal-time::get-soft-time) 123456789))

(define-test get-hard-time :parent softime
  (finish (soft-universal-time::get-hard-time))
  (let ((ht (soft-universal-time::get-hard-time))
        (ut (get-universal-time)))
    ;; hard time in millisecond
    (is = 3 (- (floor (log ht 10)) (floor (log ut 10))))))

(define-test get-hard-time* :parent softime
  (finish (soft-universal-time::get-hard-time*))
  (let ((ht (soft-universal-time::get-hard-time*))
        (ut (get-universal-time)))
    ;; hard time in millisecond
    (is = 3 (- (floor (log ht 10)) (floor (log ut 10))))
    (is = ht (soft-universal-time::get-soft-time))))

(define-test update-soft-time :parent softime
  (of-type fixnum (soft-universal-time::update-soft-time))
  (soft-universal-time::set-soft-time 0)
  (is = (soft-universal-time::update-soft-time) (soft-universal-time::get-soft-time)))

(define-test *time-getter* :parent softime
  (soft-universal-time::set-time-policy :hard)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-hard-time)
  (of-type fixnum (funcall soft-universal-time::*time-getter*))
  (soft-universal-time::set-time-policy :soft)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-soft-time)
  (of-type fixnum (funcall soft-universal-time::*time-getter*)))

(define-test initialize/restart/shutdown :parent softime
  (finish (soft-universal-time::shutdown-soft-time))

  ;; initialize
  (finish (soft-universal-time::initialize-soft-time))
  (true (soft-universal-time::soft-time-enabled-p))
  (sleep 0.2) ; default resolution 0.05
  (true (> (soft-universal-time::get-soft-time) 0))
  (true soft-universal-time::*soft-time-maintainer*)

  ;; shutdown
  (finish (soft-universal-time::shutdown-soft-time))
  (false (soft-universal-time::soft-time-enabled-p))
  (sleep 0.2)
  (is = 0 (soft-universal-time::get-soft-time))
  (is eq nil soft-universal-time::*soft-time-maintainer*)

  ;; test restart
  (finish (soft-universal-time::restart-soft-time))
  (sleep 0.2)
  (let ((time0 (soft-universal-time::get-soft-time)))
    (true (> time0 0))
    (finish (soft-universal-time::restart-soft-time))
    (sleep 0.2)
    (true (> (soft-universal-time::get-soft-time) time0)))

  (soft-universal-time::shutdown-soft-time)
  (is = 0 (soft-universal-time::get-soft-time))
  (is eq nil soft-universal-time::*soft-time-maintainer*))

(define-test maintain-hard-time :parent softime
  (let ((resolution 0.1))
    (finish (soft-universal-time::set-soft-time-resolution resolution)) ; set to a small number
    (finish (soft-universal-time::shutdown-soft-time))
    (sleep 0.5) ; wait to join the maintainer thread
    (finish (soft-universal-time::set-time-policy :hard))
    (let ((time0 (soft-universal-time::get-hard-time)))
      (finish (soft-universal-time::set-soft-time time0))
      (is = time0 (soft-universal-time::get-soft-time))
      ;; get-hard-time* will invoke set-soft-time
      (is = (soft-universal-time::get-hard-time*) (soft-universal-time::get-soft-time))
      (sleep resolution) ;; sleep so that it will not get the same hard time
      (isnt = (soft-universal-time::get-hard-time) (soft-universal-time::get-soft-time)))))

(define-test maintain-soft-time-0 :parent softime
  (let ((resolution 0.1))
    (finish (soft-universal-time::set-soft-time-resolution resolution)) ; set to a small number
    (finish (soft-universal-time::set-time-policy :soft))
    (finish (soft-universal-time::restart-soft-time))
    (true (soft-universal-time::soft-time-enabled-p))
    (sleep 0.1) ; let the maintainer update the time
    (let ((time0 (soft-universal-time::get-soft-time)))
      (sleep resolution)
      (true (> time0 0))
      (true (> (soft-universal-time::get-soft-time) time0)))
    (let ((thread soft-universal-time::*soft-time-maintainer*))
      (soft-universal-time::maintain-soft-time)
      (is eq thread soft-universal-time::*soft-time-maintainer*)
      (soft-universal-time::restart-soft-time)
      (isnt eq thread soft-universal-time::*soft-time-maintainer*))))

(define-test maintain-soft-time-1 :parent softime
  (alexandria:if-let (thread (find-if (lambda (th) (equal (bt:thread-name th) "SOFT-TIME-MAINTAINER")) (bt:all-threads)))
    (progn (is eq nil (soft-universal-time::maintain-soft-time))
           (of-type bt:thread thread))
    (progn (is eq t (soft-universal-time::maintain-soft-time))
           (of-type bt:thread (find-if (lambda (th) (equal (bt:thread-name th) "SOFT-TIME-MAINTAINER")) (bt:all-threads)))))
  (of-type bt:thread soft-universal-time::*soft-time-maintainer*)
  (is eq t (soft-universal-time::soft-time-enabled-p))
  (is eq nil (soft-universal-time::maintain-soft-time))

  (is eq nil (soft-universal-time::maintain-soft-time))

  (finish (soft-universal-time::shutdown-soft-time))
  (is eq nil (soft-universal-time::soft-time-enabled-p))
  (is eq nil soft-universal-time::*soft-time-maintainer*)
  (is = 0 soft-universal-time::**soft-universal-time**)

  (finish (soft-universal-time::restart-soft-time))
  (is eq t (soft-universal-time::soft-time-enabled-p))
  (of-type bt:thread soft-universal-time::*soft-time-maintainer*)
  (true (> soft-universal-time::**soft-universal-time** 0)))
