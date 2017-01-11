;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; change-password.lisp

(in-package #:gateway)

#|
Command CHANGE-PASSWORD

This command allows the user to change the password they use to
log in to the system.

Arguments:
* OLD-PASSWORD: string containing the previous password for verification.
* NEW-PASSWORD: string containing the new password to be set.
|#

(defcommand change-password (owner connection) (:old-password :new-password)
  (check-type old-password string)
  (check-type new-password string)
  (check-type owner crown)
  (let ((player (auth connection)))
    (unless player (error 'not-logged-in))
    (unless (typep player 'player) (error 'not-authorized :command :change-password))
    (unless (password-matches-p (password player) old-password)
      (error 'authentication-failure))
    (let ((password (make-password new-password)))
      (with-lock-held ((lock player))
        (setf (password player) password)))
    (data-send connection '(:ok :change-password))))

(deftest test-command-change-password
  (let* ((username "test-username")
         (password "test-password")
         (email "test-email@email.com")
         (new-password "new-password")
         (player (%make-player username password email))
         (not-logged-in '(:error :type :not-logged-in))
         (login-ok '(:ok :login))
         (logout-ok '(:ok :logout))
         (change-password-ok '(:ok :change-password)))
    (with-crown-and-connections crown (connection) ()
      (setf (lookup username (library crown :players)) player)
      (%test connection
             `(change-password :old-password ,password :new-password ,new-password)
             not-logged-in
             `(login :username ,username :password ,password)
             login-ok
             `(change-password :old-password ,password :new-password ,new-password)
             change-password-ok
             `(logout)
             logout-ok
             `(login :username ,username :password ,new-password)
             login-ok))))
