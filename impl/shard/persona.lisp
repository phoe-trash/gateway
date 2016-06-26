;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)
(defclass persona ()
  ((id :initform (error "No ID provided.")
       :initarg :id
       :accessor id
       :type integer)
   (%name :initform ""
	  :initarg :name
	  :accessor name
	  :type string)
   (%player :initform nil
	    :initarg :player
	    :accessor player
	    :type (or null player))
   (%avatar :initform nil
	    :initarg :avatar
	    :accessor avatar)
   (%gender :initform nil
	    :initarg :gender
	    :accessor gender
	    :type keyword)
   (%species :initform nil
	     :initarg :species
	     :accessor species
	     :type keyword)
   (%colors :initform nil
	    :initarg :colors
	    :accessor colors)
   (%shard :initform nil
	   :initarg :shard
	   :accessor shard
	   :type (or null shard))))

(defprint persona
  (princ "<" stream)
  (princ (player obj) stream)
  (princ ":" stream)
  (princ (name obj) stream) 
  (princ ">" stream))

(defconstructor (persona)
  (let ((duplicate-name (find-persona (name persona))))
    (cond (duplicate-name
	   (error "Duplicate persona name."))
	  (t
	   (with-lock-held (*cache-lock*)
	     (push persona *persona-cache*))))))

(defmethod location ((persona persona))
  (find-persona persona shard))

(defmethod send-message ((message message) (persona persona)) ;; TODO
  (send-message message (player persona)))

(defmethod find-persona ((name string))
  (find name *persona-cache* :test #'name))
