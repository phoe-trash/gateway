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
  (defgeneric (setf player) object)
  (defgeneric temp-player (object))
  (defgeneric (setf temp-player) (new-value object))
  (defgeneric chats (object))
  (defgeneric (setf chats) (object)))
