;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; command.lisp

(in-package #:gateway)

#|
Protocol COMMAND

A command is a message in form of an S-expression describing a task
to be done by gems. Each command may contain arbitrary Lisp objects.
|#
(defprotocol command ()
  (defun execute (command)
    (%execute command)))
