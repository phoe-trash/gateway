;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass message ()
  ((%sender :initform nil
	    :initarg :sender
	    :accessor sender
	    :type (or nil persona keyword))
   (%recipient :initform nil
	       :initarg :recipient
	       :accessor recipient
	       :type (or nil persona chat keyword))
   (%date :initform (now)
	  :initarg :date
	  :accessor date-of
	  :type date)
   (%contents :initform ""
	      :initarg :contents
	      :accessor contents
	      :type string)))

(defprint message
  (print-unreadable-object (obj stream :type t)
    (princ (sender obj) stream)
    (princ " → " stream)
    (princ (recipient obj) stream)
    (princ " " stream)
    (format stream "\"~A\"" (contents obj))))

(defmethod msg (sender recipient (contents string))
  (make-instance 'message
		 :sender sender
		 :recipient recipient
		 :contents contents
		 :date (get-unix-time)))
