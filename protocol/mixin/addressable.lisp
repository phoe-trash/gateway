;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/addressable.lisp

(in-package #:gateway/protocol)

(define-protocol addressable
    (:documentation "The ADDRESSABLE protocol describes objects which have a ~
network address. That address may be exported as a human-readable string."
     :tags (:addressable)
     :export t)
  (:class addressable () ())
  "An addressable object. See protocol ADDRESSABLE for details."
  (:function address ((addressable addressable)) string)
  "Returns the address of the addressable object socket in a string form.")

(execute-protocol addressable)
