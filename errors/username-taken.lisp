;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; username-taken.lisp

(in-package #:gateway)

#|
Error USERNAME-TAKEN

Should be signaled when the user provides an username which is already
registered to a player on the system.
|#

(define-gateway-error username-taken
    ((username :accessor username-taken-username
               :initarg :username
               :initform (error "Must provide username.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((username (username-taken-username condition)))
    (data-send connection `(:error :type :username-taken :username ,username))))
