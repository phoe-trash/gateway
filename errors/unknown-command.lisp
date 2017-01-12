;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; unknown-command.lisp

(in-package #:gateway)

#|
Error UNKNOWN-COMMAND

Should be signaled when the system attempts to execute a command that is
not present on the system.

This error is usually not signaled from within the command context, but from
the operation context and should be signaled by invoking the function
HANDLE-GATEWAY-ERROR on a condition instance.

Arguments:
* COMMAND: the command not found on the server.
|#

(define-gateway-error unknown-command
    ((command :accessor unknown-command-command
              :initarg :command
              :initform (error "Must provide command.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((command (unknown-command-command condition)))
    (data-send connection `(:error :type :unknown-command :command ,command))))
