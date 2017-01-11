;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; invalid-email.lisp

(in-package #:gateway)

#|
Error EMAIL-TAKEN

Should be signaled when the user provides an email which is already
registered to a player on the system.
|#

(define-gateway-error email-taken
    ((email :accessor email-taken-email
            :initarg :email
            :initform (error "Must provide email.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((email (email-taken-email condition)))
    (data-send connection `(:error :type :email-taken :email ,email))))
