;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; command.lisp

(in-package #:gateway)

#|
Protocol COMMAND

An command is a message in form of an S-expression describing a message
which has come from a remote client. Each command may contain arbitrary
SEXPABLE objects in their SEXP form.
|#
(defprotocol command ()
  (defun execute-command (owner connection command plist)
    (%execute-command owner connection command plist)))
