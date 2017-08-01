;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/killable.lisp

(in-package :gateway/protocols)

(define-protocol killable
    (:description "The KILLABLE protocol describes objects which have two ~
states: alive and dead. A killable object is always created alive and may be ~
killed in any of the cases:
* the object terminates naturally as a part of its standard functioning,
* due to an external event, e.g. a programmer's request or termination of its ~
parent object,
* when an internal error occurs from which the object cannot recover."
     :tags (:killable) :export t)
  (:class killable () ())
  "A killable object."
  (:function kill ((object killable)) :generalized-boolean)
  "If the object is alive, this function kills it. In any case, this functions ~
has no return values."
  (:function deadp ((object killable)) :generalized-boolean)
  "Returns true if the object is dead, false otherwise"
  (:function alivep ((object killable)) :generalized-boolean)
  "Returns true if the object is alive, false otherwise.
This function is a convenience function equivalent to NOT DEADP. No class
is required to define methods for it.")

(defmethod alivep (object)
  (not (deadp object)))
