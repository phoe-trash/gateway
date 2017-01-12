;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; new.lisp

(in-package #:gateway)

#|
Protocol class PERSONA

Must be NAMED and SEXPABLE.
|#
(defprotocol persona
    (persona () ())
  (defgeneric name (object))
  (defgeneric (setf name) (new-value object))
  (defgeneric player (object))
  (defgeneric (setf player) (new-value object))
  (defgeneric temp-player (object))
  (defgeneric (setf temp-player) (new-value object))
  (defgeneric chats (object))
  (defgeneric (setf chats) (new-value object)))

(defclass standard-persona (persona)
  ((%name :accessor name
          :initarg :name
          :initform "Must provide name.")
   (%player :accessor player
            :initarg :player
            :initform nil)
   (%temp-player :initform nil)
   (%chats :accessor chats
           :initform ())))

(defmethod temp-player ((persona standard-persona))
  (values (player persona) nil))

(defmethod (setf temp-player) (new-value (persona standard-persona))
  (error "Not implemented yet."))
