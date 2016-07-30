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
  (sexp `(#:message #:sender ,(sexp (sender message))
		    #:recipient ,(sexp (recipient message))
		    #:date ,(sexp (date message))
		    #:contents ,(contents message))))

(defmethod msg (sender recipient contents &key (date (now)))
  (make-instance 'standard-message :sender sender
				   :recipient recipient
				   :contents contents
				   :date date))
