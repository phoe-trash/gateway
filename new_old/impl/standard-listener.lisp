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

(defmethod alivep ((listener standard-listener))
  (thread-alive-p (thread listener)))

(defmethod kill ((listener standard-listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (values))

(defun %listener (listener)
  (format t "[~~] ~A-listener: starting.~%" (symbol-name (%type listener)))
  (unwind-protect
       (%listener-thread listener)
    (kill listener)
    (format t "[!] ~A-listener: thread killed.~%" (symbol-name (%type listener)))))

(defun %listener-thread (listener)
  (restart-case
      (loop (apply #'%listener-loop listener (%listener-type listener)))
    (retry ()
      :report "Abort the current iteration and send the listener back to its loop."
      (format t "[!] ~A-listener: restart invoked.~%" (symbol-name (%type listener)))
      (%listener-thread listener))))

(defun %listener-type (listener)
  (let ((owner (owner listener)))
    (case (%type listener)
      (:n (list (n-connections owner) (n-lock owner) :n))
      (:e (list (e-connections owner) (e-lock owner) :e))
      (:i (list (i-connections owner) (i-lock owner) :i)))))

(defun %listener-loop (listener connections lock type)
  (let* ((connection (%find-ready-connection connections lock)))
    (if connection
        (%listener-handler type listener connection)
        (sleep 0.01))))

(defun %find-ready-connection (connections lock)
  (with-lock-held (lock)
    (find-if #'readyp connections)))

(defun %listener-handler (type listener connection)
  (let* ((queue (event-queue (owner listener)))
         (data (receive connection))
         (entry (list type connection data)))
    (when data
      (enqueue entry queue))))

