;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; execute-command.lisp

(in-package #:gateway)

#|
Operation EXECUTE-COMMAND

This operation executes a command on the provided crown object
and data received from a connection.

Arguments:
* CROWN: a crown that the connection is meant to execute on.
* COMMAND: the command to be executed.
* CONNECTION: the connection from which the command was received.
|#
(defoperation execute-command (:crown :command :connection)
  (execute-command crown command connection))
