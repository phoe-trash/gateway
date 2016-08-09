;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-message.lisp

(in-package #:gateway)

(defclass standard-message (message)
  ((%sender :initarg :sender
	    :initform nil
	    :reader sender)
   (%recipient :initarg :recipient
	       :initform nil
	       :reader recipient)
   (%date :initarg :date
	  :initform (now)
	  :reader date
	  :type date)
   (%contents :initarg :contents
	      :initform ""
	      :reader contents
	      :type string)))

(defmethod sexp ((message standard-message)) 
  (sexp `(#:message #:sender ,(sender message)
		    #:recipient ,(recipient message)
		    #:date ,(date message)
		    #:contents ,(contents message))))

(defmethod msg (sender recipient contents &key (date (now)))
  (make-instance 'standard-message :sender sender
				   :recipient recipient
				   :contents contents
				   :date date))

(defmethod message= ((message-1 standard-message) (message-2 standard-message))
  (and (eq (sender message-1) (sender message-2))
       (eq (recipient message-1) (recipient message-2))
       (date= (date message-1) (date message-2))
       (string= (contents message-1) (contents message-2))))

