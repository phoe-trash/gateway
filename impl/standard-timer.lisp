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
         (setf tick 0.1))
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
  (format nil "Gateway - timer (~F ms)" (* 1000 (tick timer))))

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
      (unless (negative-real-p ms)
        (sleep ms)))))

(defun %make-timer (events tick pusher)
  (make-instance 'standard-timer :events events :tick tick :pusher pusher))



(deftest test-standard-timer-death
  (let* ((pusher (lambda (x) (declare (ignore x))))
         (timer (make-instance 'standard-timer :pusher pusher)))
    (is (alivep timer))
    (kill timer)
    (is (wait () (deadp timer)))))

(deftest test-standard-timer-speed
  (finalized-let* ((count 0)
                   (pusher (lambda (x) (incf count x)))
                   (timer (%make-timer '(1) 0.001 pusher)
                          (kill timer)))
    (is (wait () (> count 100)))))

(deftest test-standard-timer-pause
  (finalized-let* ((count 0)
                   (pusher (lambda (x) (incf count x)))
                   (timer (%make-timer '(1) 0.001 pusher)
                          (kill timer)))
    (is (wait () (> count 100)))
    (pause timer)
    (let ((temp-count count))
      (sleep 0.01)
      (is (= count temp-count))
      (unpause timer)
      (sleep 0.01)
      (is (wait () (> count temp-count))))))

(deftest test-standard-timer
  (finalized-let* ((elements ())
                   (pusher (lambda (x) (unless ( < 10 (length elements))
                                         (push x elements))))
                   (timer (%make-timer '(foo bar) 0.01 pusher)
                          (kill timer)))
    (is (wait () (equal elements
                        '(bar foo bar foo bar foo bar foo bar foo))))))
