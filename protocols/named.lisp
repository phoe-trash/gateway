;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/named.lisp

(in-package :gateway/protocols)

(define-protocol named
    (:description "The NAMED protocol describes objects which have a name - a ~
human-readable string that denotes that object's identity, along with other ~
attributes of that object."
     :tags (:named) :export t)
  (:class named () ())
  "A named object."
  (:function name ((object named)) string)
  "Returns the name of the target object.")
