;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; logout.lisp

(in-package #:gateway)

#|
Command LOGOUT

This command allows the user to log out of the system.
|#

(defcommand logout (owner connection) ()
  (check-type owner crown)
  (let ((auth (auth connection)))
    (unless auth (error 'not-logged-in)))
  (setf (auth connection) nil)
  (data-send connection '(:ok :logout)))

(deftest test-command-logout
  (let* ((username "test-username")
         (password "test-password")
         (player (%make-player username password))
         (not-logged-in '(:error :type :not-logged-in))
         (login-ok '(:ok :login))
         (logout-ok '(:ok :logout)))
    (with-crown-and-connections crown (connection) ()
      (setf (lookup username (library crown :players)) player)
      (data-send connection `(logout))
      (is (wait () (data-equal (data-receive connection) not-logged-in)))
      (data-send connection `(login :username ,username :password ,password))
      (is (wait () (data-equal (data-receive connection) login-ok)))
      (data-send connection `(logout))
      (is (wait () (data-equal (data-receive connection) logout-ok)))
      (data-send connection `(logout))
      (is (wait () (data-equal (data-receive connection) not-logged-in))))))
