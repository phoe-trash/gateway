;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; not-authorized.lisp

(in-package #:gateway)

#|
Error NOT-AUTHORIZED

Should be signaled when the user tries to perform an action he is not
authorized to perform.

Arguments:
* COMMAND: the symbol describing the command the user was not authorized
to perform.
|#

(define-gateway-error not-authorized
    ((connection :reader not-authorized-connection
                 :initarg :connection
                 :initform (error "Must provide connection."))
     (command :reader not-authorized-command
              :initarg :command
              :initform (error "Must provide the command.")))
    (owner connection condition)
    (((command (not-authorized-command condition))
      (connection (not-authorized-connection condition)))
     ("Attempted to perform an unauthorized command ~S from connection ~S."
      command connection)
      (declare (ignore owner))
      (data-send connection `(:error :type :not-authorize :command ,command))))
