;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; timer.lisp

(in-package #:gateway)

#|
Protocol class TIMER

Must be KILLABLE and NAMED.

Each timer during construction accepts a handler function and
a list of arguments. During every cycle of the timer, the handler
function is called with each of the arguments by means of
(MAPC HANDLER ARGUMENTS).
|#

(defprotocol timer
    (timer () ())
  (defgeneric name (object))
  (defgeneric pausedp (object))
  (defgeneric (setf pausedp) (new-value object))
  (defgeneric thread (object))
  (defgeneric (setf thread) (new-value object))
  (defgeneric arguments (object))
  (defgeneric (setf arguments) (new-value object))
  (defgeneric tick (object))
  (defgeneric (setf tick) (new-value object))
  (defgeneric handler (object))
  (defgeneric (setf handler) (new-value object))
  (defgeneric pause (object))
  (defgeneric unpause (object))
  (defgeneric alivep (object))
  (defgeneric kill (object)))
