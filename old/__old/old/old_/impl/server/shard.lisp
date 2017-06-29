;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass shard ()
  ((%id :accessor id
	:type integer)
   (%name :initform ""
	  :initarg :name
	  :accessor name
	  :type string)
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

(defconstructor (shard)
  (setf (id shard) (assign-id shard)))
