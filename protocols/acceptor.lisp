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
\
The handler function is a one-argument function that is meant to accept a ~
connection object as its argument and pass it to other parts of the program.
\
The acceptor, when instantiated, automatically begins handling client ~
connections in a way defined by the implementing class."
     :tags (:acceptor)
     :dependencies (killable named connection)
     :export t)
  (:class acceptor (killable named) ())
  "An acceptor object. See protocol ACCEPTOR for details."
  (:function handler ((acceptor acceptor)) function)
  "Returns the handler function of the acceptor."
  (:function (setf handler) (new-value (acceptor acceptor)) new-value)
  "Sets the handler function of the acceptor."
  (:function address ((acceptor acceptor)) string)
  "Returns the address of the acceptor's listener socket in a string form.")
