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
   (%email :initarg :name
	   :initform ""
	   :reader email
	   :type string)
   (%password :initarg :password
	      :initform nil
	      :reader password
	      :type password)
   (%personas :initform ()
	      :accessor personas 
	      :type list)))

(defmethod sexp ((player standard-player))
  `(:player :name ,(name player)))

(defmethod send-message ((message message) (player standard-player))
  (when (not (eq (recipient message) player))
    (format t "[!] Warning: recipient mismatch.~%"))
  (format t "[!] Stub: SEND-MESSAGE to ~A:~%~A~%" player (sexp message)))

(defmethod add-persona ((persona persona) (player standard-player))
  (pushnew persona (personas player)))

(defmethod delete-persona ((persona persona) (player standard-player))
  (deletef (personas player) persona))

(defmethod find-player ((name string))
  (gethash name *player-cache*))

