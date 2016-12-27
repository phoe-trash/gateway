;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; acceptor.lisp

(in-package #:gateway)

#|
Protocol class ACCEPTOR

Must be KILLABLE.

Spawns a thread which accepts new connections on the
provided host and port.
|#
(defprotocol acceptor
    (acceptor () ()))

#|
Protocol class LISTENER

Must be KILLABLE.

Spawns a thread which monitors a group of connections for activity
and pushes messages received from them onto a provided queue.
|#
(defprotocol listener
    (listener () ()))
