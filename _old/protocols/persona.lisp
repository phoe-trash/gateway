;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; persona.lisp

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
  (defgeneric owner (object))
  (defgeneric (setf owner) (new-value object))
  (defgeneric chats (object))
  (defgeneric (setf chats) (new-value object))
  (defgeneric lock (object)))
