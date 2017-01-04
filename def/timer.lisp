;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; timer.lisp

(in-package #:gateway)

#|
Protocol class TIMER

This is a class that accepts a list of events in the form ()
|#
(defprotocol timer
    (timer () ())
  (defgeneric thread (object))
  (defgeneric events (object))
  (defgeneric (setf events) (object))
  (defgeneric tick (object))
  (defgeneric (setf tick) (object))
  (defgeneric pusher (object)))
