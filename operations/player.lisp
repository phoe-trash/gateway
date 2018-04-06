;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/player.lisp

(in-package :gateway/operations)

;; TODO test, error-handling
(defun add-player (login email display-name passphrase &optional activatep)
  "Adds the player with the provided data into the database and optionally
activates them. Returns the newly created player's database ID."
  (check-type login string)
  (check-type email string)
  (check-type display-name string)
  (check-type passphrase string)
  ;; TODO remove password object from Gateway
  (multiple-value-bind (hash salt) (derive-key passphrase)
    (let ((id (gateway/sql:insert-player :login login :email email
                                         :display-name display-name
                                         :hash hash :salt salt)))
      (when activatep (activate-player :id id))))
  t)

;; TODO test, error-handling
(defun activate-player
    (&key (login nil loginp) (email nil emailp) (id nil idp))
  "Activates the player who matches the provided data. Only one of :LOGIN,
:EMAIL, or :ID must be provided."
  (assert (xor loginp emailp idp) ()
          "Must provide exactly one of :LOGIN, :EMAIL, :ID.")
  (when loginp (check-type login string))
  (when emailp (check-type email string))
  (when idp (check-type id positive-integer))
  (unless idp
    (let* ((columns (pomo:list-columns :player))
           (position (position "player_id" columns :test #'string=))
           (player (cond (loginp (gateway/sql:select-player-by-login login))
                         (emailp (gateway/sql:select-player-by-email email)))))
      (assert (not (null player)) ()
              "The player with the provided ~A ~S was not found in database."
              (cond (loginp "login") (emailp "email"))
              (cond (loginp login) (emailp email)))
      (setf id (nth position player))))
  (if (gateway/sql:activated-player-p-by-id id)
      (v:warn "Attempted to activate already active player with id ~A." id)
      (gateway/sql:activate-player-by-id id)))
