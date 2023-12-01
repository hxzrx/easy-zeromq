(defpackage :soft-universal-time
  (:use :cl)
  (:nicknames :softime)
  (:export
   :*time-getter*
   :get-time-policy
   :set-time-policy
   :get-software-time-resolution
   :set-software-time-resolution
   :initialize-software-time
   :shutdown-software-time
   :restart-software-time
   :software-time-enabled-p
   :get-software-time
   :get-hardware-time))

;; Hardware time is the time which is got from the OS directly,
;; Software time is the time which is got from a variable and updated periodically from the OS by another thread.
;; Software time is suitable for such works that time will be fetched very intensively
;; but not care much about the accuracy.
;;
;; Use case:
;; If a system can tolerate the time error >= 1 millisecond,
;; the thread will get the time from the OS for 1000 times each second and update the software time variable,
;; Getting the time from a variable can be about 100 times faster than from the OS,
;; if the system gets time much more than 1000 times, software time may be a good candidate.

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

  (declaim (single-float *software-time-resolution*))
  (defparameter *software-time-resolution* 0.005
    "Interval second for updating the software time variable.
This resolution should be less than the resolution of a timer, if the later is used in the same system.")

  (defparameter *time-policy* :software
    "Depends on how to get the current time. Can be either :hardware or :software.
If nactor-utils:maintain-software-time is called, change this var to :software."))


(defun get-software-time-resolution ()
  "Get the software time resolution in seconds."
  *software-time-resolution*)

(defun set-software-time-resolution (&optional (seconds 0.005))
  "Set the software time resolution in seconds."
  (declare (real seconds))
  (setf *software-time-resolution* (coerce seconds 'single-float)))

(declaim (inline get-software-time))
(defun get-software-time () ; cost 0.27 seconds for 10^9 times
  "Get the universal milliseconds from the glocal variable, ultra fast."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum **soft-universal-time**))

(declaim (inline set-software-time))
(defun set-software-time (milliseconds)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum milliseconds))
  (setf **soft-universal-time** milliseconds))

(declaim (inline get-hardware-time))
(defun get-hardware-time () ; cost 18 seconds for 10^9 times
  "Get the universal time from the OS, in millisecond, will not set the software time var."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (the fixnum (+ (the fixnum (* (+ (the fixnum sec) #.+unix-epoch+)
                                  #.+milliseconds-per-second+))
                   (truncate (/ (the fixnum nsec) 1000000))))))

(declaim (inline get-hardware-time*))
(defun get-hardware-time* ()
  "Get and return the universal time (in millisecond) from the OS and set the software time var."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (let ((now (the fixnum (+ (the fixnum (* (+ (the fixnum sec) #.+unix-epoch+)
                                             #.+milliseconds-per-second+))
                              (truncate (/ (the fixnum nsec) 1000000))))))
      (set-software-time now))))

(declaim (inline update-software-time))
(defun update-software-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (get-hardware-time*))

(defparameter *time-getter*
  (cond ((eq :hardware *time-policy*)
         #'get-hardware-time)
        ((eq :software *time-policy*)
         #'get-software-time)
        (t #'get-hardware-time))
  "Funcall this function object to get the current time according to the current time policy.")

(defun get-time-policy ()
  "Get the current time policy, return :software or :hardware."
  *time-policy*)

(defun set-time-policy (&optional (policy :software))
  "Set time policy, should be :software or :hardware."
  (if (or (eq policy :software) (eq policy :hardware))
      (prog1 (setf *time-policy* policy)
        (setf *time-getter*
              (cond ((eq :hardware *time-policy*)
                     #'get-hardware-time)
                    ((eq :software *time-policy*)
                     #'get-software-time)
                    (t #'get-hardware-time))))
      (error "Failed to set time policy, ~d is not valid, should be :software or :hardware." policy)))


(defvar *software-time-maintainer* nil
  "A thread which update software time periodically if *time-policy* is :software.")

(let ((%software-time-enabled-p nil))
  (defun maintain-software-time ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (unless %software-time-enabled-p
      (setf %software-time-enabled-p t)
      (setf *software-time-maintainer*
            (bt:make-thread (lambda ()
                              (loop do (progn (update-software-time)
                                              (sleep *software-time-resolution*))
                                       ;;while (software-time-enabled-p)))
                                    while %software-time-enabled-p))
                            :name "SOFTWARE-TIME-MAINTAINER"))
      ;;(sleep 0.1)
      (format t "Software time mainter thread started.~%")
      t))
  (defun software-time-enabled-p ()
    %software-time-enabled-p)
  (defun shutdown-software-time ()
    "Destroy software time maintainer thread and set %software-time-enabled-p flat to nil."
    (if  %software-time-enabled-p
         (progn
           (format t "The software time is shutting down! The thread will be destroyed and the value of software time will set to zero. You can invoke RESTART-SOFTWARE-TIME if you want to use software time later.~%")
           ;;(bt:destroy-thread *software-time-maintainer*)
           (setf %software-time-enabled-p nil)
           (when (> *software-time-resolution* 1)
             (format t "Pleast wait less than ~d seconds to shutdown software time maintainer.~%" *software-time-resolution*))
           (bt:join-thread *software-time-maintainer*)
           (setf *software-time-maintainer* nil
                 **soft-universal-time** 0))
         (format t "The software time is not enabled!~%")))
  (defun initialize-software-time ()
    "Make a thread to update the software time."
    (maintain-software-time))
  (defun restart-software-time ()
    (format t "The software time is going to restart!~%")
    (ignore-errors (shutdown-software-time))
    (maintain-software-time)))
