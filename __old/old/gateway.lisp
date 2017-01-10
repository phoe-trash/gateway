;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-listener.lisp

(in-package #:gateway)

(defclass standard-listener (listener)
  ((%thread :accessor thread)
   (%owner :accessor owner)
   (%type :initarg :type
          :accessor %type)))

(defconstructor (standard-listener owner)
  (check-type (%type standard-listener) (member :n :e :i))
  (setf (owner standard-listener) owner
        (thread standard-listener)
        (make-thread (lambda () (%listener standard-listener))
                     :name (format nil "Gateway - ~A-listener for ~S"
                                   (symbol-name (%type standard-listener)) owner))))

(defun %listener (listener)
  (format t "[~~] Listener: starting.~%")
  (unwind-protect
       (%listener-thread listener)
    (kill listener)
    (format t "[!] Listener: thread killed.~%")))

(defun %listener-thread (listener)
  (restart-case
      (let ((args (%listener-type listener)))
        (apply #'%listener-loop listener args))
    (retry ()
      :report "Abort the current iteration and send the listener back to its loop."
      (format t "[!] Listener: restart invoked.~%")
      (%listener-thread listener))))

(defun %listener-type (listener)
  (let ((owner (owner listener)))
    (case (%type (owner listener))
      (:n (list (n-connections owner) (n-lock owner) #'%listener-n-handler))
      (:e (list (e-connections owner) (e-lock owner) #'%listener-e-handler))
      (:i (list (i-connections owner) (i-lock owner) #'%listener-i-handler)))))

(defun %listener-loop (listener connections lock handler-function)
  (let* ((connection (%find-ready-connection connections lock)))
    (funcall handler-function listener connection)))

(defun %find-ready-connection (connections lock)
  (declare (ignore connections lock))
  nil)

(defun %listener-n-handler (listener connection)
  (declare (ignore connection listener))
  nil)

(defun %listener-e-handler (listener connection)
  (declare (ignore connection listener))
  nil)

(defun %listener-i-handler (listener connection)
  (declare (ignore connection listener))
  nil)
















(defmethod alivep ((listener standard-listener))
  (thread-alive-p (thread listener)))

(defmethod kill ((listener standard-listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread listener))
  (values))
