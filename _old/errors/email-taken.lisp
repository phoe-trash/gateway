;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; invalid-email.lisp

(in-package #:gateway)

#|
Error EMAIL-TAKEN

Should be signaled when the user provides an email which is already
registered to a player on the system.

Arguments:
* EMAIL: the taken email.
|#

(define-gateway-error email-taken
    ((email :reader email-taken-email
            :initarg :email
            :initform (error "Must provide email.")))
    (owner connection condition)
    (((email (email-taken-email condition)))
     ("The email ~A is already taken." email)
      (declare (ignore owner))
      (data-send connection `(:error :type :email-taken :email ,email))))
