;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; varia.lisp

(in-package #:gateway/utils)

(defun count-digits (integer)
  "Returns the number of digits in an integer, sans any sign."
  (if (= 0 integer)
      1
      (values (ceiling (log (abs integer) 10)))))

(defun data-getf (plist indicator)
  "Acts like GETF, except its keys must be symbols that are STRING= to the
INDICATOR."
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (and (symbolp key) (string= key indicator))
          return value))

(defvar *gateway-pprint-dispatch* (copy-pprint-dispatch nil))

(defvar *original-pprint-dispatch* (copy-pprint-dispatch nil))

(set-pprint-dispatch
 'string
 (lambda (stream object)
   (let ((*print-pprint-dispatch* *original-pprint-dispatch*))
     (prin1 object stream)))
 9001 *gateway-pprint-dispatch*)

(defun prinr-to-string (object)
  (let ((*print-pprint-dispatch* *gateway-pprint-dispatch*))
    (princ-to-string object)))
