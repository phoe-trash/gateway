;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; authentication-failure.lisp

(in-package #:gateway)

#|
Error AUTHENTICATION-FAILURE

Should be signaled when the user attempts to login, but the credentials
provided by him are not valid.
|#

(define-gateway-error authentication-failure
    ((connection :accessor authentication-failure-connection
                 :initarg :connection
                 :initform (error "Must provide connection.")))
    (owner connection condition)
    (((connection (authentication-failure-connection condition)))
     ("Authentication failure on connection ~A." connection)
      (declare (ignore owner))
      (data-send connection `(:error :type :authentication-failure))))

