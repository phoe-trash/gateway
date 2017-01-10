;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-logger.lisp

(in-package #:gateway)

(defclass standard-logger (logger)
  ((%thread :accessor thread)
   (%name :accessor name)
   (%stream :accessor stream-of)
   (%queue :accessor queue
           :initform (make-queue))))

(defconstructor (standard-logger)
  (let ((name "Gateway - Logger")
        (setter (lambda ()
                  (setf (stream-of standard-logger) *standard-output*))))
    (join-thread (make-thread setter))
    (setf (name standard-logger) name
          (thread standard-logger)
          (make-thread (curry #'%logger-loop standard-logger)
                       :name name))))

(defun %logger-loop (logger)
  (with-thread-handlers (logger)
    (let (*print-right-margin*)
      (apply #'fformat (stream-of logger) (pop-queue (queue logger)))
      (fresh-line))))

(defun %note (args)
  (when (and (boundp '*logger*) *logger* (alivep *logger*))
    (push-queue args (queue *logger*))))

(defmethod alivep ((logger standard-logger))
  (thread-alive-p (thread logger)))

(defmethod kill ((logger standard-logger))
  (unless (eq (current-thread) (thread logger))
    (destroy-thread (thread logger)))
  (values))

(defvar *logger* (make-instance 'standard-logger))
