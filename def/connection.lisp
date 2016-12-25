;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; connection.lisp

(in-package #:gateway)

#|
Protocol class CONNECTION

Must be KILLABLE.

A connection is an object that abstracts the actual connection
between two computers over a network. A Gateway connection is
able to #'DATA-SEND arbitrary SEXPABLE data and #'DATA-RECEIVE
it. Each connection must also implement READYP generic function
that evaluates to true if the connection has any data available
for reading and to false otherwise.

There are four types of connections:
  :LISTEN - a server connection which acccepts incoming client
connections.
  :ACCEPT - an accepted incoming connection.
  :CLIENT - an outgoing connection.
  :READY - an established connection of any type.

Constructor arguments:
  :TYPE - one of :LISTEN, :ACCEPT, :CLIENT, :READY.
  :HOST - hostname.
  :PORT - port.
|#
(defprotocol connection
    (connection () ())
  (defgeneric data-send (connection object))
  (defgeneric data-receive (connection))
  (defgeneric readyp (connection)))

#|
Protocol class ACCEPTOR

Must be KILLABLE.
|#
(defprotocol acceptor
    (acceptor () ()))
