;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; already-logged-in.lisp

(in-package #:gateway)

#|
Error ALREADY-LOGGED-IN

Should be signaled when the user tries to perform a LOGIN command while
already being logged in.

Arguments:
* AUTH: the NAMED object representing whomever the user is already logged
in as.
|#

(define-gateway-error already-logged-in
    ((auth :reader already-logged-in-auth
           :initarg :auth
           :initform (error "Must provide previous auth.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((name (name (already-logged-in-auth condition))))
    (data-send connection `(:error :type :already-logged-in :name ,name))))
