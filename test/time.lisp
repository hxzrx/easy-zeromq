(in-package :soft-universal-time-tests)


(define-test get-time-policy :parent softime
  (is eq (soft-universal-time::get-time-policy) soft-universal-time::*time-policy*))

(define-test set-time-policy :parent softime
  (finish (soft-universal-time::set-time-policy :hardware))
  (is eq (soft-universal-time::get-time-policy) :hardware)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-hardware-time)

  (finish (soft-universal-time::set-time-policy :software))
  (is eq (soft-universal-time::get-time-policy) :software)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-software-time)

  (fail (soft-universal-time::set-time-policy :xx)))

(define-test get-software-time-resolution :parent softime
  (is equal (soft-universal-time::get-software-time-resolution) soft-universal-time::*software-time-resolution*))

(define-test set-software-time-resolution :parent softime
  (let ((res1 0.6)
        (res2 2/3)
        (res3 3))
    (finish (soft-universal-time::set-software-time-resolution res1))
    (is = (soft-universal-time::get-software-time-resolution) (coerce res1 'single-float))

    (finish (soft-universal-time::set-software-time-resolution res2))
    (is = (soft-universal-time::get-software-time-resolution) (coerce res2 'single-float))

    (finish (soft-universal-time::set-software-time-resolution res3))
    (is = (soft-universal-time::get-software-time-resolution) (coerce res3 'single-float))))

(define-test get-software-time :parent softime
  (is = (soft-universal-time::get-software-time) soft-universal-time::**soft-universal-time**))

(define-test set-software-time :parent softime
  (finish (soft-universal-time::set-software-time 0))
  (is = (soft-universal-time::get-software-time) 0)

  (finish (soft-universal-time::set-software-time 123456789))
  (is = (soft-universal-time::get-software-time) 123456789))

(define-test get-hardware-time :parent softime
  (finish (soft-universal-time::get-hardware-time))
  (let ((ht (soft-universal-time::get-hardware-time))
        (ut (get-universal-time)))
    ;; hardware time in millisecond
    (is = 3 (- (floor (log ht 10)) (floor (log ut 10))))))

(define-test get-hardware-time* :parent softime
  (finish (soft-universal-time::get-hardware-time*))
  (let ((ht (soft-universal-time::get-hardware-time*))
        (ut (get-universal-time)))
    ;; hardware time in millisecond
    (is = 3 (- (floor (log ht 10)) (floor (log ut 10))))
    (is = ht (soft-universal-time::get-software-time))))

(define-test update-software-time :parent softime
  (of-type fixnum (soft-universal-time::update-software-time))
  (soft-universal-time::set-software-time 0)
  (is = (soft-universal-time::update-software-time) (soft-universal-time::get-software-time)))

(define-test *time-getter* :parent softime
  (soft-universal-time::set-time-policy :hardware)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-hardware-time)
  (of-type fixnum (funcall soft-universal-time::*time-getter*))
  (soft-universal-time::set-time-policy :software)
  (is eq soft-universal-time::*time-getter* #'soft-universal-time::get-software-time)
  (of-type fixnum (funcall soft-universal-time::*time-getter*)))

(define-test initialize/restart/shutdown :parent softime
  (finish (soft-universal-time::shutdown-software-time))

  ;; initialize
  (finish (soft-universal-time::initialize-software-time))
  (true (soft-universal-time::software-time-enabled-p))
  (sleep 0.2) ; default resolution 0.05
  (true (> (soft-universal-time::get-software-time) 0))
  (true soft-universal-time::*software-time-maintainer*)

  ;; shutdown
  (finish (soft-universal-time::shutdown-software-time))
  (false (soft-universal-time::software-time-enabled-p))
  (sleep 0.2)
  (is = 0 (soft-universal-time::get-software-time))
  (is eq nil soft-universal-time::*software-time-maintainer*)

  ;; test restart
  (finish (soft-universal-time::restart-software-time))
  (sleep 0.2)
  (let ((time0 (soft-universal-time::get-software-time)))
    (true (> time0 0))
    (finish (soft-universal-time::restart-software-time))
    (sleep 0.2)
    (true (> (soft-universal-time::get-software-time) time0)))

  (soft-universal-time::shutdown-software-time)
  (is = 0 (soft-universal-time::get-software-time))
  (is eq nil soft-universal-time::*software-time-maintainer*))

(define-test maintain-hardware-time :parent softime
  (let ((resolution 0.1))
    (finish (soft-universal-time::set-software-time-resolution resolution)) ; set to a small number
    (finish (soft-universal-time::shutdown-software-time))
    (sleep 0.5) ; wait to join the maintainer thread
    (finish (soft-universal-time::set-time-policy :hardware))
    (let ((time0 (soft-universal-time::get-hardware-time)))
      (finish (soft-universal-time::set-software-time time0))
      (is = time0 (soft-universal-time::get-software-time))
      ;; get-hardware-time* will invoke set-software-time
      (is = (soft-universal-time::get-hardware-time*) (soft-universal-time::get-software-time))
      (sleep resolution) ;; sleep so that it will not get the same hardware time
      (isnt = (soft-universal-time::get-hardware-time) (soft-universal-time::get-software-time)))))

(define-test maintain-software-time :parent softime
  (let ((resolution 0.1))
    (finish (soft-universal-time::set-software-time-resolution resolution)) ; set to a small number
    (finish (soft-universal-time::set-time-policy :software))
    (finish (soft-universal-time::restart-software-time))
    (true (soft-universal-time::software-time-enabled-p))
    (sleep 0.1) ; let the maintainer update the time
    (let ((time0 (soft-universal-time::get-software-time)))
      (sleep resolution)
      (true (> time0 0))
      (true (> (soft-universal-time::get-software-time) time0)))
    (let ((thread soft-universal-time::*software-time-maintainer*))
      (soft-universal-time::maintain-software-time)
      (is eq thread soft-universal-time::*software-time-maintainer*)
      (soft-universal-time::restart-software-time)
      (isnt eq thread soft-universal-time::*software-time-maintainer*))))
