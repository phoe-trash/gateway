;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/named.lisp

(in-package #:gateway/protocol)

(define-protocol named
    (:documentation "The NAMED protocol describes objects which have a name - a ~
human-readable string that denotes that object's identity, along with other ~
attributes of that object."
     :tags (:named)
     :export t)
  (:class named () ())
  "A named object. See protocol NAMED for details."
  (:function name ((object named)) string)
  "Returns the name of the target object.")

(execute-protocol named)
