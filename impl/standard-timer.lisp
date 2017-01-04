;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-timer.lisp

(in-package #:gateway)

(defclass standard-timer (timer)
  ((%thread :accessor thread)
   (%arguments :accessor arguments :initarg :arguments :initform ())
   (%tick :accessor tick)
   (%handler :accessor handler)
   (%paused :accessor pausedp)))

(defconstructor (standard-timer tick handler pausedp)
  (cond ((null tick)
         (setf tick 100.0))
        ((not (floatp tick))
         (setf tick (float tick))))
  (setf (tick standard-timer) tick
        (handler standard-timer) handler
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
        (mapc (handler timer) (arguments timer)))
      (setf after-time (get-internal-real-time)
            time-diff (float (* (- after-time before-time)
                                internal-time-units-per-second))
            ms (/ (- (tick timer) time-diff) 1000.0))
      (unless (negative-real-p ms)
        (sleep ms)))))

(defun %make-timer (arguments tick handler)
  (make-instance 'standard-timer :arguments arguments :tick tick :handler handler))



(deftest test-standard-timer-death
  (let* ((handler (lambda (x) (declare (ignore x))))
         (timer (make-instance 'standard-timer :handler handler)))
    (is (alivep timer))
    (kill timer)
    (is (wait () (deadp timer)))))

(deftest test-standard-timer-speed
  (finalized-let* ((count 0)
                   (handler (lambda (x) (incf count x)))
                   (timer (%make-timer '(1) 1.0 handler)
                          (kill timer)))
    (is (wait () (> count 100)))))

(deftest test-standard-timer-pause
  (finalized-let* ((count 0)
                   (handler (lambda (x) (incf count x)))
                   (timer (%make-timer '(1) 1.0 handler)
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
  (finalized-let*
      ((elements ())
       (handler (lambda (x) (unless (<= 10 (length elements))
                              (push x elements))))
       (timer (%make-timer '(foo bar) 10.0 handler)
              (kill timer)))
    (is (wait () (equal elements '(bar foo bar foo bar
                                   foo bar foo bar foo))))))
