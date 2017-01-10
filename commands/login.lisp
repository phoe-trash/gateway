;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; login.lisp

(in-package #:gateway)

#|
Command LOGIN

This command allows the user to log into the system using the
provided username and password.

Arguments:
* USERNAME: string containing the username.
* PASSWORD: string containing the password.
|#

(defcommand login (owner connection) (:username :password)
  (check-type username string)
  (check-type password string)
  (check-type owner crown)
  (let ((auth (auth connection)))
    (when auth (error 'already-logged-in :auth auth)))
  (let ((player (lookup username (library owner :players))))
    (unless (and (string= username (username player))
                 (password-matches-p password (password player)))
      (error 'authentication-failure))
    (setf (auth connection) player)
    (data-send connection `(:login-ok :username ,username))))

#|
Protocol class PLAYER

Must be NAMED.
|#
(defprotocol player
    (player () ())
  (defgeneric username (player))
  (defgeneric password (player)))

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
