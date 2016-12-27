;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; listener.lisp

(in-package #:gateway)

#|
Protocol class LISTENER

Must be KILLABLE.

Spawns a thread which monitors a group of connections for activity
and pushes messages received from them onto a provided queue.
|#
(defprotocol listener
    (listener () ()))
