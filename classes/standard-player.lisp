;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-player.lisp

(in-package #:gateway)

(defclass standard-player (player)
  ((%username :accessor username
              :initarg :username
              :initform (error "Must provide username."))
   (%password :accessor password
              :initarg :password
              :initform (error "Must provide password."))))

(defmethod name ((player standard-player))
  (username player))

(defun %make-player (username passphrase)
  (make-instance 'standard-player :username username
                                  :password (make-password passphrase)))
