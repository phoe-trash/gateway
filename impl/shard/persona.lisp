;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)
(defclass persona ()
  ((%name :initform ""
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

(defmethod location ((persona persona))
  (find-persona persona shard))

#|
(let* ((world-map (world-map (shard persona)))
       (dimensions (array-dimensions world-map))
       (size (reduce #'* dimensions)))
  (iter (for i from 0 to (1- size))
    (when (eq persona (row-major-aref world-map i))
      (return-from location (values i dimensions))))))
|#

(defmethod send-message ((message message) (persona persona)) ;; TODO
  (send-message message (player persona)))

