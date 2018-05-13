;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/with-handler.lisp

(in-package #:gateway/protocol)

(define-protocol with-handler
    (:documentation "The WITH-HANDLER protocol describes objects which have a
handler function.
\
This protocol specifies only a mechanism of accessing the handler function and
does not concern itself with the arity or arguments of the handler function
itself."
     :tags (:with-handler)
     :export t)
  (:class with-handler () ())
  "An object with a handler. See protocol WITH-HANDLER for details."
  (:function handler ((object with-handler)) function)
  "Returns the handler function of the object."
  (:function (setf handler) (new-value (object with-handler)) t)
  "Sets the handler function of the object.")

(execute-protocol with-handler)
