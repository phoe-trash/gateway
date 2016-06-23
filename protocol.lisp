;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defaccessors sender recipient date-of contents
  name player avatar gender species colors shard
  name messages personas shard
  id username password email personas connection
  world-map jewel personas chats lock)

;;;; shard protocol

;; message.lisp
(defspecialization sender (message))
(defspecialization recipient (message))
(defspecialization date-of (message))
(defspecialization contents (message))
(defspecialization msg (sender recipient contents))


;; persona.lisp
(defspecialization name (persona))
(defspecialization player (persona))
(defspecialization avatar (persona))
(defspecialization gender (persona))
(defspecialization species (persona))
(defspecialization colors (persona))

(defspecialization shard (persona))
(defspecialization location (persona))

(defspecialization send-message (message persona))


;; chat.lisp
(defspecialization name (chat))
(defspecialization messages (chat))
(defspecialization personas (chat))
(defspecialization shard (chat))

(defspecialization send-message (message chat))
(defspecialization find-messages (chat &key sender recipient after-date before-date contents))
(defspecialization delete-message (message chat))

(defspecialization add-persona (persona chat))
(defspecialization delete-persona (persona chat))


;; password.lisp
(defspecialization make-password (passphrase))
(defspecialization password-matches-p (password passphrase))


;; world-map.lisp
(defspecialization dimensions (world-map))
(defspecialization x-dimension (world-map))
(defspecialization y-dimension (world-map))
(defspecialization object-at (world-map x y))
(defspecialization resize (world-map up left down right &key initial-element))


;; player.lisp
(defspecialization id (player))
(defspecialization username (player))
(defspecialization password (player))
(defspecialization email (player))
(defspecialization personas (player))
(defspecialization connection (player))
(defspecialization find-player (&key id username email))
(defspecialization send-message (message player))


;;;; server protocol

;; shard.lisp
(defspecialization world-map (shard))
(defspecialization jewel (shard))
(defspecialization personas (shard))
(defspecialization chats (shard))
(defspecialization lock (shard))
(defspecialization find-persona (persona shard))


;; connection.lisp
(defspecialization output (object connection))
(defspecialization input (connection))
