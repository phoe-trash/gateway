;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/listener.lisp

(in-package #:gateway/protocols)

(define-protocol listener
    (:description "The LISTENER protocol describes objects which contain a ~
list of connections and constantly scan them for incoming data, reading it ~
and calling their handler function on them to pass it to other parts of the ~
system.

The connection list should be treated as immutable at all time and should ~
never be destructively modified. Instead, all operations are expected to copy ~
the list and call (SETF CONNECTIONS).

The handler function is a two-argument function that is meant to accept a ~
connection object and the data that came from that connection as its arguments ~
and pass it to other parts of the program.

The listener, when instantiated, automatically begins handling client ~
connections in a way defined by the implementing class."
     :tags (:listener)
     :dependencies (acceptor connection killable named)
     :export t)
  (:class listener (killable named) ())
  "A listener object."
  (:function lock ((listener listener)) lock)
  "Retrieves the lock of the listener.

It is an error to call CONNECTIONS or (SETF CONNECTIONS) without this lock ~
being held."
  (:function connections ((listener listener)) list)
  "Retrieves the connections list of the listener."
  (:function (setf connections) (new-value (listener listener)) new-value)
  "Sets the connections list of the listener and notifies it about the change, ~
so, if the listener is waiting, the next iteration of its functionality begins ~
automatically with the newly provided connection list."
  (:function handler ((listener listener)) function)
  "Returns the handler function of the listener."
  (:function (setf handler) (new-value (listener listener)) new-value)
  "Sets the handler function of the listener.")
