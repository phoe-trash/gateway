;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; invalid-username.lisp

(in-package #:gateway)

#|
Error INVALID-USERNAME

Should be signaled when the user provides an username which is not
a valid username address.

Arguments:
* USERNAME: the invalid username.
|#

(define-gateway-error invalid-username
    ((username :reader invalid-username-username
               :initarg :username
               :initform (error "Must provide username.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((username (invalid-username-username condition)))
    (data-send connection `(:error :type :invalid-username :username ,username))))
