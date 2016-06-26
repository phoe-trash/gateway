;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass shard ()
  ((id :initform (error "No ID provided.")
       :initarg :id
       :accessor id
       :type integer)
   (%world-map :initform (error "No world map provided.")
	       :initarg :world-map
	       :accessor world-map
	       :type world-map)
   (%jewel :initform (error "No jewel provided.")
	   :initarg :jewel
	   :accessor jewel
	   :type jewel)
   (%personas :initform nil
	      :initarg :personas
	      :accessor personas
	      :type list)
   (%chats :initform nil
	   :initarg :chats
	   :accessor chats
	   :type list)
   (%lock :initform (make-lock)
	  :accessor lock)))
