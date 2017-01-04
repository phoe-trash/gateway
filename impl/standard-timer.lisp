;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-timer.lisp

(in-package #:gateway)

(defclass standard-timer (timer)
  ((%thread :accessor thread)
   (%events :accessor events :initform ())
   (%tick :accessor tick)
   (%pusher :accessor pusher)
   (%paused :accessor %pausedp :initform nil)))

(defconstructor (standard-timer tick pusher)
  (cond ((null tick)
         (setf tick 100.0))
        ((and (numberp tick) (not (floatp tick)))
         (setf tick (float tick)))
        (t
         (error "Tick value must be a number.")))
  (setf (tick standard-timer) tick
        (pusher standard-timer) pusher
        (thread standard-timer)
        (make-thread (curry #'%timer-loop standard-timer)
                     :name (%timer-name standard-timer))))

(defmethod name ((timer standard-timer))
  (%timer-name timer))

(defun %timer-name (timer)
  (format nil "Gateway - timer (~F ms)" (tick timer)))

(defun %timer-loop (timer)
  (with-thread-handlers (timer)
    (let (before-time after-time time-diff)
      (setf before-time (get-internal-real-time))
      (unless (%pausedp timer)
        (mapc (pusher timer) (events timer)))
      (setf after-time (get-internal-real-time)
            time-diff (float (* (- after-time before-time)
                                internal-time-units-per-second)))
      (sleep (- (tick timer) time-diff)))))
