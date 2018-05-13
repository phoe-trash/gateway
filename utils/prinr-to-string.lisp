;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/prinr-to-string.lisp

(in-package #:gateway/utils)

(defvar *gateway-pprint-dispatch* (copy-pprint-dispatch nil))

(defvar *original-pprint-dispatch* (copy-pprint-dispatch nil))

(set-pprint-dispatch
 'null
 (lambda (stream object)
   (declare (ignore object))
   (write-char #\( stream)
   (write-char #\) stream))
 9001 *gateway-pprint-dispatch*)

(set-pprint-dispatch
 'string
 (lambda (stream object)
   (let ((*print-pprint-dispatch* *original-pprint-dispatch*))
     (prin1 object stream)))
 9001 *gateway-pprint-dispatch*)

(defun prinr-to-string (object)
  (let ((*print-pprint-dispatch* *gateway-pprint-dispatch*))
    (princ-to-string object)))

(defun prinr (object &optional stream)
  (let ((*print-pprint-dispatch* *gateway-pprint-dispatch*))
    (princ object stream)))
