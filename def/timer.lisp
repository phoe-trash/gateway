;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; timer.lisp

(in-package #:gateway)

#|
Protocol class TIMER

Must be KILLABLE and NAMED.

This is a class that accepts a list of events that are repeatedly
pushed using a provided pusher function.
|#
(defprotocol timer
    (timer () ())
  (defgeneric name (object))
  (defgeneric pausedp (object))
  (defgeneric (setf pausedp) (new-value object))
  (defgeneric thread (object))
  (defgeneric events (object))
  (defgeneric (setf events) (new-value object))
  (defgeneric tick (object))
  (defgeneric pusher (object))
  (defgeneric pause (object))
  (defgeneric unpause (object))
  (defgeneric alivep (object))
  (defgeneric kill (object)))
