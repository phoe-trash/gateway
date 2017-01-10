;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; killable.lisp

(in-package #:gateway)

#|
Protocol KILLABLE

Each KILLABLE object has two states: alive and dead. Such an
object is created alive by default and can be killed in any
moment in two cases:
* due to an external event, such as killing its parent object
or a programmer's request,
* when an internal error occurs, causing the object to fail
serving its purpose.
|#

(defprotocol killable ()
  (defgeneric kill (object))
  (defgeneric alivep (object))
  (defun deadp (object) (not (alivep object))))
