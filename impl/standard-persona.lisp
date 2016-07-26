;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-persona.lisp

(in-package #:gateway)

(defclass standard-persona (persona)
  ((%name :initarg :name
	  :initform (error "Attempted to create a persona with an empty name.")
	  :reader name
	  :type string)
   (%player :initarg :player
	    :initform (error "Attempted to create a persona without a player.")
	    :reader player
	    :type player) 
   (%chat :initarg :chat
	  :initform nil
	  :reader chat
	  :type chat)))

(defmethod sexp ((persona standard-persona))
  `(:persona :name ,(name persona)))

(defmethod send-message ((message message) (recipient standard-persona))
  (send-message message (player recipient)))

(defmethod find-persona ((name string))
  (gethash name *persona-cache*))
