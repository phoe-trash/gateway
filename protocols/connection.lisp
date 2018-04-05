;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/acceptor.lisp

(in-package :gateway/protocols)

(define-protocol connection
    (:description "The CONNECTION protocol describes objects representing ~
network connections to individual remote machines. These connections are able ~
to send and receive messages in form of reduced S-expressions (S-expressions ~
which contain only proper lists, uninterned symbols, strings, numbers and ~
serializable objects, which are serialized before sending).
\
Each such object has an authentication slot."
     :tags (:connection)
     :dependencies (killable serializable)
     :export t)
  (:class connection (killable) ())
  "A connection object. See protocol CONNECTION for details."
  (:function authentication ((connection connection)) t)
  "Returns the implementation-dependent object representing the user that ~
authenticated themselves using this connection."
  (:function (setf authentication) (new-value (connection connection)) t)
  "Sets the implementation-dependent object representing the user that ~
authenticated themselves using this connection."
  (:function connection-send ((connection connection) object) boolean)
  "Sends the provided reduced S-expression through the connection and returns ~
T if the sending succeeded. Otherwise (e.g. if the connection is dead, as by ~
#'DEADP), this function returns NIL."
  (:function connection-receive ((connection connection)) (values list boolean))
  "Attempts to receive a message from the provided connection. If a message ~
is successfully retrieved, this function returns (VALUES MESSAGE T), where ~
MESSAGE is the returned message without any deserialization.
\
If there is no full message waiting on the connection, this function returns ~
\(VALUES NIL T.)
\
If the provided connection is dead (as by DEADP), this function returns ~
\(VALUES NIL NIL)."
  (:function readyp ((connection connection)) :generalized-boolean)
  "Returns true if there is any data waiting to be received on the provided ~
connection, including a possible end-of-file condition; otherwise, returns ~
false.
\
This data does not necessarily need to constitute a full message; it is ~
possible for a connection to be READYP but for CONNECTION-RECEIVE to return ~
\(VALUES NIL T), at which point the received partial data is buffered, the ~
connection becomes not READYP again, and only a subsequent part of the message ~
arriving on the connection causes CONNECTION-RECEIVE to return a full message."
  (:function address ((connection connection)) string)
  "Returns the address of the connection's listener socket in a string form."
  (:function ready-connection-using-class ((concrete-class class) connections)
             (connection (or null connection)))
  "Provided with a concrete connection class and a list of instances of that ~
class, blocks until a connection has available data, at which point that ~
connection is returned.
\
Dead connections are automatically removed from CONNECTIONS during the ~
blocking period of this function. If the list of connections becomes empty as ~
an effect of this, or an implementation's timeout expires, this function ~
returns NIL instead."
  (:function ready-connection (connections) (connection (or null connection)))
  "Calls READY-CONNECTION-USING-CLASS using the class of the first element of ~
CONNECTIONS.")

(defmethod ready-connection (connections)
  (ready-connection-using-class (class-of (first connections)) connections))

(defmethod ready-connection ((connections null)) nil)

(defmethod ready-connection-using-class (class (connections null)) nil)
