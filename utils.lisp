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
