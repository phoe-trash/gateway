;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; activate-player.lisp

(in-package #:gateway)

#|
Operation ACTIVATE-PLAYER

This operation activates a player who has registered on the server.

Arguments:
* CROWN: the crown the player registered on.
* USERNAME: username of the player to activate.
|#

(defoperation activate-player (:crown :username)
  (with-lock-held ((inactive-players-lock crown))
    (let ((player (find username (inactive-players crown) :key #'username)))
      (unless player
        (error 'unknown-player :username username))
      (setf (lookup username (library crown :players)) player
            (lookup (email player) (library crown :emails)) player)
      (deletef (inactive-players crown) player :count 1))))

(deftest test-operation-activate-player
  (let* ((username "test-username")
         (password "test-password")
         (email "test-email@email.com")
         (player (%make-player username password email)))
    (with-crown-and-connections crown () ()
      (with-lock-held ((inactive-players-lock crown))
        (push player (inactive-players crown)))
      (execute-operation 'activate-player :crown crown :username username)
      (let ((player-lookup (lookup username (library crown :players))))
        (is (eq player player-lookup))
        (with-lock-held ((inactive-players-lock crown))
          (is (null (inactive-players crown))))))))
