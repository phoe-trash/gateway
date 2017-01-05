;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-player.lisp

(in-package #:gateway)

(defclass standard-player (player)
  ((%name :initarg :name
	  :initform (error "Attempted to create a player with an empty name.")
	  :reader name
	  :type string)
   (%email :initarg :email
	   :initform ""
	   :reader email
	   :type string)
   (%password :initarg :password
	      :initform nil
	      :reader password
	      :type password)
   (%personas :initform ()
	      :accessor personas 
	      :type list)
   (%connection :initarg :connection
		:initform nil
		:reader connection
		:type connection)))

(defconstructor (standard-player)
  (if (cache :player (name standard-player))
      (error "A player by name ~S already exists." (name standard-player))
      (setf (cache :player (name standard-player)) standard-player)))

(defmethod sexp ((player standard-player))
  (sexp `(#:player #:name ,(name player))))

(defmethod send-message ((message message) (player standard-player))
  (if (connection player)
      (send (connection player) message)
      (format t "[!] Stub: SEND-MESSAGE to ~A:~%~A~%" player (sexp message))))

(defmethod add-persona ((persona persona) (player standard-player))
  (pushnew persona (personas player)))

(defmethod delete-persona ((persona persona) (player standard-player))
  (deletef (personas player) persona))

(defmethod find-player ((name string))
  (cache :player name))

