;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/acceptor.lisp

(in-package :gateway/protocols)

(define-protocol acceptor
    (:description "The ACCEPTOR protocol describes objects which accept ~
incoming client connections (objects of protocol class CONNECTION) and call a ~
handler function on that connection, so it may later be handled by other parts ~
of the program .

The acceptor, when instantiated, automatically begins handling client ~
connections in a way defined by the implementing class."
     :tags (:acceptor)
     :dependencies (killable named connection)
     :export t)
  (:class acceptor (killable named) ())
  "An acceptor object."
  (:function handler ((acceptor acceptor)) function)
  "Returns the handler function of the acceptor."
  (:function (setf handler) (new-value (acceptor acceptor)) function)
  "Sets the handler function of the acceptor.")
