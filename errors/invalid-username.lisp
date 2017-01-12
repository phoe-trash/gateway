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
    (((username (invalid-username-username condition)))
     ("The provided username, ~S, was invalid." username)
      (declare (ignore owner))
      (data-send connection `(:error :type :invalid-username :username ,username))))
