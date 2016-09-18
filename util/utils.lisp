;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; utils.lisp

(in-package #:gateway)

(defun string=-getf (plist indicator)
  (loop for key in plist by #'cddr
	for value in (rest plist) by #'cddr
	when (and (string= key indicator))
	  return value))

(defun fformat (stream format-string &rest format-args)
  (apply #'format stream format-string format-args)
  (force-output stream))

(defun make-synchro-queue ()
  (make-instance 'synchronized-queue :queue (make-instance 'unbounded-fifo-queue)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cat (&rest strings)
    (apply #'concatenate 'string strings)))

(defun peek-char-no-hang (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((character (read-char-no-hang input-stream eof-error-p eof-value recursive-p)))
    (when character
      (unread-char character input-stream)
      character)))
