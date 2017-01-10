;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; authentication-failure.lisp

(in-package #:gateway)

#|
Error AUTHENTICATION-FAILURE

Should be signaled when the user attempts to login, but the credentials
provided by him are not valid.
|#

(define-gateway-error authentication-failure ()
    (owner connection condition)
  (declare (ignore owner condition))
  (data-send connection `(:error :type :authentication-failure))
  (note "[!] Authentication failure completed.~%"))
