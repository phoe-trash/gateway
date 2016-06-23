;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass player ()
  ((id :initform (error "No ID provided.")
       :initarg :id
       :accessor id
       :type integer)
   (username :initform ""
	     :initarg :username
	     :accessor username
	     :type string)
   (password :initform nil
	     :initarg :password
	     :accessor password
	     :type (or password null))
   (email :initform ""
	  :initarg :email
	  :accessor email
	  :type string)
   (personas :initform nil
	     :initarg :personas
	     :accessor personas
	     :type list)
   (connection :initform nil
	       :initarg :connection
	       :accessor connection)))

(defmethod send-message ((message message) (player player))
  (output message (connection player)))

(defmethod find-player (&key id username email)
  (cond ((not (only-one-p id username email))
	 (error "This function requires exactly one key argument."))
	(id
	 (%player-by-id id))
	(username
	 (%player-by-username username))
	(email
	 (%player-by-email email))))

(defun %player-by-id (id)
  (todo "%player-by-id"))

(defun %player-by-username (username)
  (todo "%player-by-username"))

(defun %player-by-email (email)
  (todo "%player-by-email"))
