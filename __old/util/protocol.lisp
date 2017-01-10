;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocol.lisp

(in-package #:gateway)

(defprotocol messagable ()
  (defgeneric send-message (message recipient)))

(defprotocol player
    (player () ()
      (:documentation "Must be SEXPABLE, IDENTIFIABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:EMAIL (optional) - a STRING.
:PASSWORD - a PASSWORD or a passphrase (a STRING).
"))
  (defgeneric name (object))
  (defgeneric email (object))
  (defgeneric password (object))
  (defgeneric connection (object))
  (defgeneric personas (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object))
  (defgeneric find-player (name)))

(defprotocol persona
    (persona () ()
      (:documentation "Must be SEXPABLE, IDENTIFIABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:PLAYER - a PLAYER.
:CHAT - a CHAT."))
  (defgeneric name (object))
  (defgeneric player (object))
  (defgeneric chat (object))
  (defgeneric find-persona (name)))

(defprotocol message
    (message () ()
      (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:SENDER - the sender of the message.
:RECIPIENT - the recipient of the message.
:DATE - object of type DATE.
:CONTENTS - contents of the message (a STRING)."))
  (defgeneric sender (object))
  (defgeneric recipient (object))
  (defgeneric date (object))
  (defgeneric contents (object))
  (defgeneric msg (sender recipient contents &key date))
  (defgeneric message= (message-1 message-2)))

(defprotocol chat
    (chat () ()
      (:documentation "Must be SEXPABLE, IDENTIFIABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING."))
  (defgeneric name (object))
  (defgeneric messages (object))
  (defgeneric personas (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object))
  (defgeneric find-chat (name)))

(defprotocol shard
    (shard () ())

  (defgeneric players (object))
  (defgeneric chats (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object)))

(defprotocol connector
    (connector () ()
      (:documentation "Constructor arguments:
:HOST - host to try to connect to,
:PORT - port to try to connect to,
:COOKIE - magic cookie to expect from the crown.
"))
  (defgeneric alivep (object))
  (defgeneric kill (object)))

(defprotocol jewel
    (jewel () ())
  ;; EVENT QUEUE
  (defgeneric event-queue (object))
  (defgeneric gems (object))
  (defgeneric timer (object))
  ;; CONNECTION
  (defgeneric connection (object))
  (defgeneric connector (object))
  ;; SHARDS
  (defgeneric shards (object))
  (defgeneric shards-lock (object))
  (defgeneric add-shard (jewel shard))
  (defgeneric remove-shard (jewel shard))
  ;; METHODS
  (defgeneric kill (object))
  (defgeneric alivep (object)))




