;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-timer.lisp

(in-package #:gateway)

(defclass standard-timer (timer)
  ((%thread :accessor thread)
   (%events :accessor events :initarg :events :initform ())
   (%tick :accessor tick)
   (%pusher :accessor pusher)
   (%paused :accessor pausedp)))

(defconstructor (standard-timer tick pusher pausedp)
  (cond ((null tick)
         (setf tick 100))
        ((and (numberp tick) (not (floatp tick)))
         (setf tick (float tick)))
        ((not (floatp tick))
         (error "Tick value must be a number.")))
  (setf (tick standard-timer) tick
        (pusher standard-timer) pusher
        (pausedp standard-timer) pausedp
        (thread standard-timer)
        (make-thread (curry #'%timer-loop standard-timer)
                     :name (%timer-name standard-timer))))

(defmethod name ((timer standard-timer))
  (%timer-name timer))

(defun %timer-name (timer)
  (format nil "Gateway - timer (~F ms)" (tick timer)))

(defmethod pause ((timer standard-timer))
  (unless (pausedp timer)
    (setf (pausedp timer) t)
    t))

(defmethod unpause ((timer standard-timer))
  (when (pausedp timer)
    (setf (pausedp timer) nil)
    t))

(defmethod alivep ((timer standard-timer))
  (thread-alive-p (thread timer)))

(defmethod kill ((timer standard-timer))
  (unless (eq (current-thread) (thread timer))
    (destroy-thread (thread timer))))

;; (defun %timer-loop (timer)
;;   (with-thread-handlers (timer)
;;     (declare (optimize speed))
;;     (let ((before-time 0) (after-time 0) (time-diff 0.0) (ms 0.0))
;;       (declare (type integer before-time after-time)
;;                (type single-float time-diff ms))
;;       (setf before-time (get-internal-real-time))
;;       (unless (pausedp timer)
;;         (mapc (the function (pusher timer)) (events timer)))
;;       (setf after-time (get-internal-real-time)
;;             time-diff (float (* (- after-time before-time)
;;                                 (float internal-time-units-per-second)))
;;             ms (* (- (the single-float (tick timer)) time-diff) 1/1000))
;;       (unless (> 0 ms)
;;         (sleep ms)))))

(defun %timer-loop (timer)
  (with-thread-handlers (timer)
    (let (before-time after-time time-diff ms)
      (setf before-time (get-internal-real-time))
      (unless (pausedp timer)
        (mapc (pusher timer) (events timer)))
      (setf after-time (get-internal-real-time)
            time-diff (float (* (- after-time before-time)
                                internal-time-units-per-second))
            ms (- (tick timer) time-diff))
      (unless (> 0 ms)
        (sleep ms)))))

(deftest test-standard-timer-death
  (let* ((pusher (lambda (x) (declare (ignore x))))
         (timer (make-instance 'standard-timer :pusher pusher)))
    (is (alivep timer))
    (kill timer)
    (is (wait () (deadp timer)))))

(deftest test-standard-timer-quantity
  (finalized-let* ((count 0)
                   (pusher (lambda (x) (incf count x)))
                   (timer (make-instance 'standard-timer
                                         :events '(1)
                                         :tick 1.0 :pusher pusher)
                          (kill timer)))
    (is (wait () (> count 100)))))
