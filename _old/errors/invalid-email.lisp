;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; invalid-email.lisp

(in-package #:gateway)

#|
Error INVALID-EMAIL

Should be signaled when the user provides an email which is not
a valid email address.

Arguments:
* EMAIL: the invalid email.
|#

(define-gateway-error invalid-email
    ((email :reader invalid-email-email
            :initarg :email
            :initform (error "Must provide email.")))
    (owner connection condition)
    (((email (invalid-email-email condition)))
     ("The provided email, ~S, was invalid." email)
      (declare (ignore owner))
      (data-send connection `(:error :type :invalid-email :email ,email))))
