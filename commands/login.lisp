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
  (multiple-value-bind (player foundp) (lookup username (library owner :players))
    (unless (and foundp
                 (string= username (username player))
                 (password-matches-p (password player) password))
      (error 'authentication-failure))
    (setf (auth connection) player)
    (data-send connection '(:ok :login))))

(deftest test-command-login
  (let* ((username "test-username")
         (password "test-password")
         (wrong-username "wrong-username")
         (wrong-password "wrong-password")
         (player (%make-player username password))
         (ok '(:ok :login))
         (authentication-failure '(:error :type :authentication-failure))
         (already-logged-in '(:error :type :already-logged-in)))
    (with-crown-and-connections crown (connection) ()
      (setf (lookup username (library crown :players)) player)
      (flet ((check (username password response)
               (data-send connection `(login :username ,username :password ,password))
               (is (wait () (data-equal (data-receive connection) response)))))
        (check wrong-username password authentication-failure)
        (check username wrong-password authentication-failure)
        (check username password ok)
        (is (find player (n-connections crown) :key #'auth))
        (check username password already-logged-in)))))
