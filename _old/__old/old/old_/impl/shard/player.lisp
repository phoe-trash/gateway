;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass player ()
  ((id :accessor id
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

(defconstructor (player)
  (setf (id player) (assign-id player))
  (let ((duplicate-id (find-player :id (id player)))
	(duplicate-username (find-player :username (username player)))
	(duplicate-email (find-player :email (email player))))
    (cond (duplicate-id
	   (error "Duplicate player ID."))
	  (duplicate-username
	   (error "Duplicate player username."))
	  (duplicate-email
	   (error "Duplicate player email."))
	  (t
	   (with-lock-held (*cache-lock*)
	     (push player *player-cache*))))))

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
	 (%player-by-email email))
	(t
	 (error "Should not end up here - FIND-PLAYER broke."))))

(defun %player-by-id (id)
  (find id *player-cache* :key #'id))

(defun %player-by-username (username)
  (find username *player-cache* :key #'username))

(defun %player-by-email (email)
  (find email *player-cache* :key #'email))
