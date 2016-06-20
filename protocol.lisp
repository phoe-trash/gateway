;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

;;;; shard protocol

;; message.lisp
(defgeneric sender (message))
(defgeneric recipient (message))
(defgeneric date-of (message))
(defgeneric contents (message))
(defgeneric msg (sender recipient contents))


;; persona.lisp
(defgeneric name (persona))
(defgeneric player (persona))
(defgeneric avatar (persona))
(defgeneric gender (persona))
(defgeneric species (persona))
(defgeneric colors (persona))

(defgeneric shard (persona))
(defgeneric location (persona))

(defgeneric send-message (message persona))


;; chat.lisp
(defgeneric name (chat))
(defgeneric messages (chat))
(defgeneric personas (chat))
(defgeneric shard (chat))

(defgeneric send-message (message chat))
(defgeneric find-messages (chat &key sender recipient after-date before-date contents))
(defgeneric delete-message (message chat))

(defgeneric add-persona (persona chat))
(defgeneric delete-persona (persona chat))


;; password.lisp
(defgeneric make-password (passphrase))
(defgeneric password-matches-p (password passphrase))


;; world-map.lisp
(defgeneric dimensions (world-map))
(defgeneric x-dimension (world-map))
(defgeneric y-dimension (world-map))
(defgeneric object-at (world-map x y))
(defgeneric resize (world-map up left down right &key initial-element))


;; player.lisp
(defgeneric id (player))
(defgeneric username (player))
(defgeneric password (player))
(defgeneric email (player))
(defgeneric personas (player))
(defgeneric connection (player))
(defgeneric find-player (&key id username email))
(defgeneric send-message (message player))


;;;; server protocol

;; shard.lisp
(defgeneric world-map (shard))
(defgeneric jewel (shard))
(defgeneric personas (shard))
(defgeneric chats (shard))
(defgeneric lock (shard))
(defgeneric find-persona (persona shard))


;; connection.lisp
(defgeneric output (object connection))
(defgeneric input (connection))
