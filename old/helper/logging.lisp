;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defparameter *logging-function* 'format-log)

(defun format-log (&rest args)
  (apply #'format t args)
  (terpri))

(defun todo (reason &rest args)
  (let ((result-string (if reason
			   (cat "[WARN] TODO: " reason)
			   "[ERROR] TODO called without a reason")))
    (apply *logging-function* result-string args)))

