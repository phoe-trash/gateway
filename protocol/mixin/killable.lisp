;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/killable.lisp

(in-package #:gateway/protocol)

(define-protocol killable
    (:documentation "The KILLABLE protocol describes objects which have two ~
states: alive and dead. A killable object is always created alive and may be ~
killed in any of the cases:
* the object terminates naturally as a part of its standard functioning,
* due to an external event, e.g. a programmer's request or termination of its ~
parent object,
* when an internal error occurs from which the object cannot recover."
     :tags (:killable)
     :export t)
  (:class killable () ())
  "A killable object. See protocol KILLABLE for details."
  (:function kill ((object killable)) (values))
  "If the object is alive, this function kills it. In any case, this functions ~
has no return values."
  (:function deadp ((object killable)) t)
  "Returns true if the object is dead, false otherwise"
  (:function alivep ((object killable)) t)
  "Returns true if the object is alive, false otherwise.
This function is a convenience function equivalent to NOT DEADP. No class
is required to define methods for it.")

(execute-protocol killable)

(defmethod alivep (object)
  (not (deadp object)))
