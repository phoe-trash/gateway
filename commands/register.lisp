;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; register.lisp

(in-package #:gateway)

#|
Command REGISTER

This command attempts to register a player with the provided username,
password and email.

Arguments:
* USERNAME: the username of the created player.
* PASSWORD: the password of the created player.
* EMAIL: the email of the created player.
|#

(defcommand register (owner connection) (:username :password :email)
  (check-type username string)
  (check-type password string)
  (check-type email string)
  (check-type owner crown)
  (let ((auth (auth connection)))
    (when auth (error 'already-logged-in :auth auth)))
  (%command-register-check-email owner email)
  (%command-register-check-username owner username)
  (let ((player (%make-player username password email)))
    (with-lock-held ((inactive-players-lock owner))
      (push player (inactive-players owner))))
  (data-send connection '(:ok :register)))

(defun %command-register-check-email (owner email)
  (unless (valid-email-p email) (error 'invalid-email :email email))
  (when (or (nth-value 1 (lookup email (library owner :emails)))
            (with-lock-held ((inactive-players-lock owner))
              (find email (inactive-players owner)
                    :key #'email :test  #'string=)))
    (error 'email-taken :email email)))

(defun %command-register-check-username (owner username)
  (unless (valid-username-p username) (error 'invalid-username :username username))
  (when (or (nth-value 1 (lookup username (library owner :players)))
            (with-lock-held ((inactive-players-lock owner))
              (find username (inactive-players owner)
                    :key #'username :test #'string=)))
    (error 'username-taken :username username)))

;; TODO test for command REGISTER
(deftest test-command-register
  (let* ((username "test-username")
         (password "test-password")
         (email "test-email@email.com")
         (wrong-username "Test Username")
         (wrong-email "test+email#email.com")
         (alt-username "test-username-2")
         (alt-email "test-email-2@email.com")
         (invalid-username `(:error :type :invalid-username :username ,wrong-username))
         (invalid-email `(:error :type :invalid-email :email ,wrong-email))
         (username-taken `(:error :type :username-taken :username ,username))
         (email-taken `(:error :type :email-taken :email ,email))
         (register-ok '(:ok :register)))
    (with-crown-and-connections crown (connection) ()
      (%test connection
             `(register :username ,wrong-username :password ,password :email ,email)
             invalid-username
             `(register :username ,username :password ,password :email ,wrong-email)
             invalid-email
             `(register :username ,username :password ,password :email ,email)
             register-ok)
      (with-lock-held ((inactive-players-lock crown))
        (is (find username (inactive-players crown) :key #'username :test #'string=)))
      (%test connection
             `(register :username ,username :password ,password :email ,alt-email)
             username-taken
             `(register :username ,alt-username :password ,password :email ,email)
             email-taken))))
