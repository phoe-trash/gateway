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
              :initform (error "Must provide password."))
   (%email :accessor email
           :initarg :email
           :initform (error "Must provide email."))
   (%lock :accessor lock)))

(defconstructor (standard-player)
  (setf (lock standard-player)
        (make-lock (format nil "Lock for player ~S" (username standard-player)))))

(defmethod name ((player standard-player))
  (username player))

(defun %make-player (username passphrase email)
  (make-instance 'standard-player :username username :email email
                                  :password (make-password passphrase)))

(defmethod sexp ((player standard-player))
  (sexp `(:player :username ,(username player))))

(defunsexp player ((username :username)) (crown)
  (assert username () "Username cannot be empty.")
  (multiple-value-bind (player foundp) (lookup username (library crown :players))
    (if (and foundp (string= username (username player)))
        player
        (error 'unknown-player :username username))))

(deftest test-standard-player
  (let* ((username "test-username")
         (password "test-password")
         (email "test-email@email.com")
         (player (%make-player username password email)))
    (with-crown-and-connections crown () ()
      (setf (lookup username (library crown :players)) player)
      (is (password-matches-p (password player) password))
      (is (eq player (unsexp (sexp player) crown))))))


