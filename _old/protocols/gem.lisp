;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gem.lisp

(in-package #:gateway)

#|
Protocol class GEM

Must be NAMED and KILLABLE.

Each gem has a thread loop which first calls a getter function to get a
message that is then processed by the handler function. If an error occurs
and push-on-error-p is non-nil, then the gem is expected to call the pusher
function that should allow the message to be processed again later. If
push-oh-error-p is nil, the error is signaled instead by means of #'ERROR.

The getter function is expected to block until a message is available.
|#

(defprotocol gem
    (gem () ())
  (defgeneric thread (object))
  (defgeneric (setf thread) (new-value object))
  (defgeneric getter (object))
  (defgeneric (setf getter) (new-value object))
  (defgeneric pusher (object))
  (defgeneric (setf pusher) (new-value object))
  (defgeneric handler (object))
  (defgeneric (setf handler) (new-value object))
  (defgeneric push-on-error-p (object))
  (defgeneric (setf push-on-error-p) (new-value object))
  (defgeneric alivep (object))
  (defgeneric kill (object)))
