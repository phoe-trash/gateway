;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; acceptor.lisp

(in-package #:gateway)

#|
Protocol class ACCEPTOR

Must be KILLABLE and NAMED.

Spawns a thread which accepts new connections on the
provided host and port.
|#
(defprotocol acceptor
    (acceptor () ()))
