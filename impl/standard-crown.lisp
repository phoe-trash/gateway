;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-crown.lisp

(in-package #:gateway)

(defclass standard-crown (crown)
  ((%persona->i-connection :accessor persona->i-connection
			   :initform (make-hash-table :test #'equal))
   (%gems :accessor gems
	  :initform nil)
   (%queue :accessor queue
	   :initform (make-synchro-queue))
   (%listener :accessor listener)
   (%n-connections :accessor n-connections
		   :initform nil)
   (%n-connections-lock :accessor n-connections-lock
			:initform (make-lock))
   (%e-connections :accessor e-connections
		   :initform nil)
   (%e-connections-lock :accessor e-connections-lock
			:initform (make-lock))
   (%i-connections :accessor i-connections
		   :initform nil) 
   (%i-connections-lock :accessor i-connections-lock
			:initform (make-lock))))

(defconstructor (standard-crown host port)
  (format t "[~~] Crown: created.~%")
  (setf (listener standard-crown)
	(make-instance 'standard-listener :host host :port port
					  :crown standard-crown))
  (setf (gems standard-crown)
	(list (make-instance 'standard-gem :parent standard-crown))))

(defmethod kill ((crown standard-crown))
  (kill (listener crown))
  (mapc #'kill (gems crown))
  (with-lock-held ((n-connections-lock crown))
    (with-lock-held ((i-connections-lock crown))
      (with-lock-held ((e-connections-lock crown)) 
	(mapc (lambda (x) (mapc #'kill x))
              (list (n-connections crown) (e-connections crown)
                    (i-connections crown))))))
  (values))
  
