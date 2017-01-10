;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; unknown-player.lisp

(in-package #:gateway)

#|
Error UNKNOWN-PLAYER

Should be signaled when a gem attempts to access a user which is not
present in the owner.
|#

(define-gateway-error unknown-player
    ((username :accessor unknown-player-username
               :initarg :username
               :initform (error "Must provide username.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((username (username (unknown-player-username condition))))
    (data-send connection `(:error :type :unknown-user :username ,username))))
