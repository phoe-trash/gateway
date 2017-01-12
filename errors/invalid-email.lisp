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
    ((email :accessor invalid-email-email
            :initarg :email
            :initform (error "Must provide email.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((email (invalid-email-email condition)))
    (data-send connection `(:error :type :invalid-email :email ,email))
    (note "[!] Finished handling INVALID-EMAIL.~%")))
