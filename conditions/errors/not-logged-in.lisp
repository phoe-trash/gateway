;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; not-logged-in.lisp

(in-package #:gateway)

#|
Error not-LOGGED-IN

Should be signaled when the user tries to perform a LOGOUT command while
not being logged in.
|#

(define-gateway-error not-logged-in ()
    (owner connection condition)
  (declare (ignore owner condition))
  (data-send connection `(:error :type :not-logged-in)))
