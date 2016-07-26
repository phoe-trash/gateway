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
	      :initform (error "Attempted to create an empty message.")
	      :reader contents
	      :type string)))

(defmethod sexp ((message standard-message)) 
  `(:message :sender ,(sender message)
	     :recipient ,(recipient message)
	     :date ,(date message)
	     :contents ,(contents message)))

(defmethod msg (sender recipient contents &key (date (now)))
  (make-instance 'message :sender sender
			  :recipient recipient
			  :contents contents
			  :date date))
