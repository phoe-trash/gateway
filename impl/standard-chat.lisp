;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-chat.lisp

(in-package #:gateway)

(defclass standard-chat (chat)
  ((%name :initarg :name
	  :initform (error "Attempted to create a chat with an empty name.")
	  :reader name
	  :type string)
   (%messages :initarg :messages
	      :initform ()
	      :accessor messages) 
   (%personas :initarg :personas
	      :initform ()
	      :accessor personas)))

(defconstructor (standard-chat)
  (setf (cache :chat (name standard-chat)) standard-chat))

(defmethod sexp ((chat standard-chat))
  (sexp `(#:chat #:name ,(name chat))))

(defmethod send-message ((message message) (chat standard-chat))
  (push message (messages chat))
  (let ((players (remove-duplicates (mapcar #'player (personas chat)))))
    (mapc (lambda (x) (send-message message x)) players)))

(defmethod add-persona ((persona persona) (chat standard-chat))
  (pushnew persona (personas chat)))

(defmethod delete-persona ((persona persona) (chat standard-chat))
  (deletef (personas chat) persona))

(defmethod find-chat ((name string))
  (gethash name *chat-cache*))
