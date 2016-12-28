;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-listener.lisp

(in-package #:gateway)

(defclass standard-listener (listener)
  ((%connection :accessor connection)
   (%thread :accessor thread)
   (%name :accessor name)
   (%conn-getter :accessor conn-getter
                 :initarg :conn-getter
                 :initform (error "Must define a connection getter function."))
   (%conn-pusher :accessor conn-pusher
                 :initarg :conn-pusher
                 :initform (error "Must define a connection pusher function."))
   (%data-pusher :accessor data-pusher
                 :initarg :data-pusher
                 :initform (error "Must define a data pusher function."))))

(defconstructor (standard-listener)
  (multiple-value-bind (connection-1 connection-2) (make-connection-pair)
    (let ((name "Gateway - Listener")
          (fn (lambda () (%listener-loop-1 standard-listener))))
      (funcall (conn-pusher standard-listener) connection-1)
      (setf (name standard-listener) "Gateway - Listener"
            (connection standard-listener) connection-2
            (thread standard-listener) (make-thread fn :name name)))))

(defun %listener-loop-1 (listener)
  (format t "[~~] ~A: starting.~%" (name listener))
  (unwind-protect
       (%listener-loop-2 listener)
    (kill listener)
    (format t "[!] ~A: killed.~%" (name listener))))

(defun %listener-loop-2 (listener)
  (restart-case
      (loop (%listener-loop-3 listener))
    (retry ()
      :report "Abort the current iteration and send the listener back to its loop."
      (format t "[!] ~A: restarted.~%" (name listener))
      (%listener-loop-2 listener))))

(defun %listener-loop-3 (listener)
  (let* ((sockets (mapcar #'socket (funcall (conn-getter listener))))
         (socket (first (wait-for-input sockets :timeout nil :ready-only t)))
         (connection (owner socket))
         (data (data-receive connection)))
    (cond (data
           (format t "[.] ~A: got data, ~S.~%" (name listener) data)
           (funcall (data-pusher listener) (list connection data)))
          (t
           (format t "[.] ~A: got notified.~%" (name listener))))))

(defmethod notify ((listener standard-listener))
  (fformat (socket-stream (socket (connection listener))) "()~%"))

(defmethod alivep ((listener standard-listener))
  (thread-alive-p (thread listener)))

(defmethod kill ((listener standard-listener))
  (kill (connection listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (values))

(deftest test-standard-listener
  (let* ((conn-getter (lambda ()))
         (conn-pusher (lambda (x) (kill x)))
         (data-pusher (lambda (x) x))
         (listener (make-instance 'standard-listener
                                  :conn-getter conn-getter :conn-pusher conn-pusher
                                  :data-pusher data-pusher)))
    (is (alivep listener))
    (kill listener)
    (is (wait () (deadp listener))))
  (let* ((connections nil) (data nil) (lock (make-lock "STANDARD-LISTENER test"))
         (sample-data '(foo bar baz quux))
         (conn-getter (lambda () (with-lock-held (lock) connections)))
         (conn-pusher (lambda (x) (with-lock-held (lock) (push x connections))))
         (data-pusher (lambda (x) (with-lock-held (lock) (push x data)))))
    (finalized-let*
        ((listener (make-instance 'standard-listener
                                  :conn-getter conn-getter :conn-pusher conn-pusher
                                  :data-pusher data-pusher)
                   (kill listener) (is (wait () (deadp listener))))
         (conns-1 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-1) (is (wait () (every #'deadp conns-1))))
         (conns-2 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-2) (is (wait () (every #'deadp conns-2)))))
      (with-lock-held (lock)
        (push (first conns-1) connections)
        (push (first conns-2) connections)
        (notify listener)
        (data-send (second conns-1) sample-data)
        (data-send (second conns-2) sample-data))
      (flet ((output-present-p (connection)
               (wait () (with-lock-held (lock)
                          (member (list connection sample-data)
                                  data :test #'data-equal)))))
        (is (output-present-p (first conns-1)))
        (is (output-present-p (first conns-2)))))))

;; Oh goodness, I remember the days when I've had no idea what a closure was
;; and how a function can be an object.
;; ~phoe, 28 Dec 2016
