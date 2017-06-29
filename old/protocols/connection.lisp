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

If :SOCKET is not supplied, the connection will attempt to
create a socket on its own, connecting to the host and port
provided in the constructor arguments.

Constructor arguments:
  :SOCKET - a socket with an associated data stream.
  :HOST - hostname.
  :PORT - port.
|#

(defprotocol connection
    (connection () ())
  (defgeneric auth (connection))
  (defgeneric stream-of (connection))
  (defgeneric data-send (connection object))
  (defgeneric data-receive (connection))
  (defgeneric readyp (connection)))
