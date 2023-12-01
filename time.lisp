(defpackage :soft-universal-time
  (:use :cl)
  (:nicknames :soft-time)
  (:export
   :*time-getter*
   :get-time-policy
   :set-time-policy
   :get-soft-time-resolution
   :set-soft-time-resolution
   :initialize-soft-time
   :shutdown-soft-time
   :restart-soft-time
   :soft-time-enabled-p
   :get-soft-time
   :get-hard-time))

;; Hard time is the time which is got from the OS directly,
;; Soft time is the time which is got from a variable and updated periodically from the OS by another thread.
;; Soft time is suitable for such works that time will be fetched very intensively
;; but not care much about the accuracy.
;;
;; Use case:
;; If a system can tolerate the time error >= 1 millisecond,
;; the thread will get the time from the OS for 1000 times each second and update the soft time variable,
;; Getting the time from a variable can be about 100 times faster than from the OS,
;; if the system gets time much more than 1000 times, soft time may be a good candidate.

(in-package :soft-universal-time)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-global (name value &optional doc)
    #+sbcl`(sb-ext:defglobal ,name ,value ,doc)
    #+ccl`(ccl:defstatic ,name ,value ,doc)
    #+:lispworks`(hcl:defglobal-variable ,name ,value ,doc)
    #-(or sbcl ccl lispworks) `(defvar ,name ,value ,doc))

  (declaim (fixnum **soft-universal-time**))
  (define-global **soft-universal-time** 0
    "Present the not very accurate universal time in milliseconds.")

  (defconstant +milliseconds-per-second+ 1000)
  (defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

  (declaim (single-float *soft-time-resolution*))
  (defparameter *soft-time-resolution* 0.005
    "Interval second for updating the soft time variable.
This resolution should be less than the resolution of a timer, if the later is used in the same system.")

  (defparameter *time-policy* :soft
    "Depends on how to get the current time. Can be either :hard or :soft.
If nactor-utils:maintain-soft-time is called, change this var to :soft."))


(defun get-soft-time-resolution ()
  "Get the soft time resolution in seconds."
  *soft-time-resolution*)

(defun set-soft-time-resolution (&optional (seconds 0.005))
  "Set the soft time resolution in seconds."
  (declare (real seconds))
  (setf *soft-time-resolution* (coerce seconds 'single-float)))

(declaim (inline get-soft-time))
(defun get-soft-time () ; cost 0.27 seconds for 10^9 times
  "Get the universal milliseconds from the glocal variable, ultra fast."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum **soft-universal-time**))

(declaim (inline set-soft-time))
(defun set-soft-time (milliseconds)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum milliseconds))
  (setf **soft-universal-time** milliseconds))

(declaim (inline get-hard-time))
(defun get-hard-time () ; cost 18 seconds for 10^9 times
  "Get the universal time from the OS, in millisecond, will not set the soft time var."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (the fixnum (+ (the fixnum (* (+ (the fixnum sec) #.+unix-epoch+)
                                  #.+milliseconds-per-second+))
                   (truncate (/ (the fixnum nsec) 1000000))))))

(declaim (inline get-hard-time*))
(defun get-hard-time* ()
  "Get and return the universal time (in millisecond) from the OS and set the soft time var."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (let ((now (the fixnum (+ (the fixnum (* (+ (the fixnum sec) #.+unix-epoch+)
                                             #.+milliseconds-per-second+))
                              (truncate (/ (the fixnum nsec) 1000000))))))
      (set-soft-time now))))

(declaim (inline update-soft-time))
(defun update-soft-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (get-hard-time*))

(defparameter *time-getter*
  (cond ((eq :hard *time-policy*)
         #'get-hard-time)
        ((eq :soft *time-policy*)
         #'get-soft-time)
        (t #'get-hard-time))
  "Funcall this function object to get the current time according to the current time policy.")

(defun get-time-policy ()
  "Get the current time policy, return :soft or :hard."
  *time-policy*)

(defun set-time-policy (&optional (policy :soft))
  "Set time policy, should be :soft or :hard."
  (if (or (eq policy :soft) (eq policy :hard))
      (prog1 (setf *time-policy* policy)
        (setf *time-getter*
              (cond ((eq :hard *time-policy*)
                     #'get-hard-time)
                    ((eq :soft *time-policy*)
                     #'get-soft-time)
                    (t #'get-hard-time))))
      (error "Failed to set time policy, ~d is not valid, should be :soft or :hard." policy)))


(defvar *soft-time-maintainer* nil
  "A thread which update soft time periodically if *time-policy* is :soft.")

(let ((%soft-time-enabled-p nil))
  (defun maintain-soft-time ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (unless %soft-time-enabled-p
      (setf %soft-time-enabled-p t)
      (setf *soft-time-maintainer*
            (bt:make-thread (lambda ()
                              (loop do (progn (update-soft-time)
                                              (sleep *soft-time-resolution*))
                                    while %soft-time-enabled-p))
                            :name "SOFT-TIME-MAINTAINER"))
      (format t "Soft time mainter thread started.~%")
      t))
  (defun soft-time-enabled-p ()
    %soft-time-enabled-p)
  (defun shutdown-soft-time ()
    "Destroy soft time maintainer thread and set %soft-time-enabled-p flat to nil."
    (if  %soft-time-enabled-p
         (progn
           (format t "The soft time is shutting down! The thread will be destroyed and the value of soft time will set to zero. You can invoke RESTART-SOFT-TIME if you want to use soft time later.~%")
           ;;(bt:destroy-thread *soft-time-maintainer*)
           (setf %soft-time-enabled-p nil)
           (when (> *soft-time-resolution* 1)
             (format t "Pleast wait less than ~d seconds to shutdown soft time maintainer.~%" *soft-time-resolution*))
           (bt:join-thread *soft-time-maintainer*)
           (setf *soft-time-maintainer* nil
                 **soft-universal-time** 0))
         (format t "The soft time is not enabled!~%")))
  (defun initialize-soft-time ()
    "Make a thread to update the soft time."
    (maintain-soft-time))
  (defun restart-soft-time ()
    (format t "The soft time is going to restart!~%")
    (ignore-errors (shutdown-soft-time))
    (maintain-soft-time)))
