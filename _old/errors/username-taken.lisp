;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; username-taken.lisp

(in-package #:gateway)

#|
Error USERNAME-TAKEN

Should be signaled when the user provides an username which is already
registered to a player on the system.

Arguments:
* USERNAME: the taken username.
|#

(define-gateway-error username-taken
    ((username :reader username-taken-username
            :initarg :username
            :initform (error "Must provide username.")))
    (owner connection condition)
    (((username (username-taken-username condition)))
     ("The username ~A is already taken." username)
      (declare (ignore owner))
      (data-send connection `(:error :type :username-taken :username ,username))))
