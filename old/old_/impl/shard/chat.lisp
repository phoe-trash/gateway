;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass chat ()
  ((id :accessor id
       :type integer)
   (%name :initform "Unnamed Chat"
	  :initarg :name
	  :accessor name
	  :type string)
   (%messages :initform nil
	      :accessor messages
	      :type list)
   (%personas :initform nil
	      :accessor personas
	      :type list)
   (%shard :initform nil
	   :initarg :shard
	   :accessor shards
	   :type (or null shard))))

(defconstructor (chat)
  (setf (id chat) (assign-id chat)))

(defprint chat
  (print-unreadable-object (obj stream :type t)
    (princ (name obj) stream)
    (princ "@" stream)
    (princ (name (shard obj)) stream)))

(defmethod send-message ((message message) (chat chat))
  (push message (messages chat))
  (format t "[~A] ~A TO ~A: ~A"
	  (name chat) (sender message) (recipient message) (contents message))
  (mapcar (lambda (persona) (send-message message persona)) (personas chat))
  message)

(defmethod find-messages (chat &key sender recipient after-date before-date contents)
  (declare (ignore after-date before-date))
  (macrolet ((%filter (by-what &key (fn 'eq) stub)
		 (if stub
		     `(declare (ignore messages))
		     `(if ,by-what 
			  (remove-if-not (lambda (x) (,fn ,by-what (,by-what x))) messages)
			  messages))))
    (flet ((by-sender (messages) (%filter sender))
	   (by-recipient (messages) (%filter recipient))
	   (by-date (messages) (%filter date :stub t))
	   (by-contents (messages) (%filter contents :fn search)))
      (let* ((fn (compose #'by-sender #'by-recipient #'by-date #'by-contents)))
	(funcall fn (messages chat))))))

(defmethod delete-message (message chat)
  (setf (messages chat) (delete message (messages chat))))

(defmethod add-persona ((persona persona) (chat chat))
  (pushnew persona (personas chat)))

(defmethod delete-persona ((persona persona) (chat chat))
  (setf (personas chat) (delete persona (personas chat))))
