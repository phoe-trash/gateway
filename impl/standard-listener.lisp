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
   (%conn-cleaner :accessor conn-cleaner
                  :initarg :conn-cleaner
                  :initform (lambda ()))
   (%data-pusher :accessor data-pusher
                 :initarg :data-pusher
                 :initform (error "Must define a data pusher function."))))

(defconstructor (standard-listener (name ""))
  (multiple-value-bind (connection-1 connection-2) (make-connection-pair)
    (let ((fn (lambda () (loop until (funcall (conn-getter standard-listener)))
                (%listener-loop standard-listener))))
      (funcall (conn-pusher standard-listener) connection-1)
      (setf (name standard-listener) (%listener-constructor-name name)
            (connection standard-listener) connection-2
            (thread standard-listener)
            (make-thread fn :name (name standard-listener))))))

(defun %listener-constructor-name (name)
  (format nil "Gateway - ~AListener" name))

(defun %listener-loop (listener)
  (with-thread-handlers (listener)
    (handler-case
        (let* ((sockets (mapcar #'socket (funcall (conn-getter listener))))
               (socket (first (wait-for-input sockets :timeout nil :ready-only t)))
               (connection (owner socket))
               (command (data-receive connection)))
          (cond (command
                 (note "[.] ~A: got a command, ~S.~%" (name listener) command)
                 (funcall (data-pusher listener) connection command))
                (t
                 (note "[.] ~A: got a notification.~%" (name listener)))))
      (stream-error (e)
        (%listener-handle-stream-error listener e)))))

(defun %listener-handle-stream-error (listener condition)
  (note "[!] ~A: stream error: ~A~%" (name listener) condition)
  (let* ((connections (funcall (conn-getter listener)))
         (stream (stream-error-stream condition))
         (predicate (lambda (x) (eq stream (socket-stream (socket x)))))
         (connection (find-if predicate connections)))
    (when connection
      (kill connection)
      (note "[!] ~A: killed the offending connection.~%" (name listener))
      (funcall (conn-cleaner listener)))))

(defun %make-listener (getter pusher data-pusher cleaner)
  (make-instance 'standard-listener
                 :conn-getter getter
                 :conn-pusher pusher
                 :data-pusher data-pusher
                 :conn-cleaner cleaner))

(defmethod notify ((listener standard-listener))
  (fformat (socket-stream (socket (connection listener))) "()~%"))

(defmethod alivep ((listener standard-listener))
  (thread-alive-p (thread listener)))

(defmethod kill ((listener standard-listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (kill (connection listener))
  (values))



(deftest test-standard-listener-death
  (let* ((conn-getter (lambda ()))
         (conn-pusher (lambda (x) (kill x)))
         (data-pusher (lambda (x y) (declare (ignore x  y))))
         (listener (%make-listener conn-getter conn-pusher data-pusher conn-getter)))
    (is (alivep listener))
    (kill listener)
    (is (wait () (deadp listener)))))

(deftest test-standard-listener-dead-connection
  (let* ((connections nil) (data nil) (lock (make-lock "STANDARD-LISTENER test"))
         (conn-getter (lambda () (with-lock-held (lock) connections)))
         (conn-pusher (lambda (x) (with-lock-held (lock) (push x connections))))
         (data-pusher (lambda (x y) (with-lock-held (lock) (push (list x y) data)))))
    (finalized-let*
        ((listener (%make-listener conn-getter conn-pusher data-pusher conn-getter)
                   (kill listener) (is (wait () (deadp listener))))
         (conns (multiple-value-list (make-connection-pair))
                (mapc #'kill conns) (is (wait () (every #'deadp conns)))))
      (with-lock-held (lock)
        (push (first conns) connections)
        (notify listener)
        (is (alivep (first conns)))
        (kill (second conns))
        (is (wait () (deadp (first conns))))))))

(deftest test-standard-listener
  (let* ((connections nil) (data nil) (lock (make-lock "STANDARD-LISTENER test"))
         (sample-data-1 '(#:foo #:bar #:baz #:quux)) (sample-data-2 '(1 2 3 #:quux))
         (conn-getter (lambda () (with-lock-held (lock) connections)))
         (conn-pusher (lambda (x) (with-lock-held (lock) (push x connections))))
         (data-pusher (lambda (x y) (with-lock-held (lock) (push (list x y) data)))))
    (finalized-let*
        ((listener (%make-listener conn-getter conn-pusher data-pusher conn-getter)
                   (kill listener) (is (wait () (deadp listener))))
         (conns-1 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-1) (is (wait () (every #'deadp conns-1))))
         (conns-2 (multiple-value-list (make-connection-pair))
                  (mapc #'kill conns-2) (is (wait () (every #'deadp conns-2)))))
      (with-lock-held (lock)
        (push (first conns-1) connections)
        (push (first conns-2) connections)
        (notify listener)
        (data-send (second conns-1) sample-data-1)
        (data-send (second conns-1) sample-data-2)
        (data-send (second conns-2) sample-data-1))
      (flet ((output-present-p (connection output)
               (wait () (with-lock-held (lock)
                          (member (list connection output) data
                                  :test #'data-equal)))))
        (is (output-present-p (first conns-1) sample-data-1))
        (is (output-present-p (first conns-1) sample-data-2))
        (is (output-present-p (first conns-2) sample-data-1))))))

;; Oh goodness, I remember the days when I've had no idea what a closure was
;; and how a function can be an object.
;; ~phoe, 28 Dec 2016
