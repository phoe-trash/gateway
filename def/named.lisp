;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; named.lisp

(in-package #:gateway)

#|
Protocol NAMED

Every NAMED class must implement generic function #'NAME which returns
a string naming the particular instance of a class.
|#
(defprotocol named ()
  (defgeneric name (object)))
