;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; operation.lisp

(in-package #:gateway)

#|
Protocol OPERATION

An operation is a message in form of an S-expression describing a task
to be done by gems. Each operation may contain arbitrary Lisp objects.
|#
(defprotocol operation ()
  (defun execute-operation (operation &rest plist)
    (%execute-operation operation plist)))
