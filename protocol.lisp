;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocol.lisp

(in-package #:gateway)


;; SEXPABLE protocol
(defprotocol sexpable () 
  (defgeneric sexp (object)))

;; MESSAGABLE protocol
(defprotocol messagable ()
  (defgeneric send-message (message recipient)))

;; MESSAGE protocol - implemented by STANDARD-MESSAGE
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
  (defgeneric msg (sender recipient contents &key date)))

;; DATE protocol - implemented by STANDARD-DATE
(defprotocol date 
    (date () ()
      (:documentation "Must be SEXPABLE and IMMUTABLE."))
  (defgeneric parse-date (string))
  (defgeneric date= (date-1 date-2 &key unit))
  (defgeneric date/= (date-1 date-2 &key unit))
  (defgeneric date< (date-1 date-2))
  (defgeneric date<= (date-1 date-2 &key unit))
  (defgeneric date> (date-1 date-2))
  (defgeneric date>= (date-1 date-2 &key unit))
  (defgeneric date-min (date-1 &rest dates))
  (defgeneric date-max (date-1 &rest dates))
  (defgeneric now ()))

;; PASSWORD protocol - implemented by STANDARD-PASSWORD
(defprotocol password
    (password () ()
      (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:PASSPHRASE - a passphrase."))
  (defgeneric password-matches-p (password passphrase))
  (defgeneric make-password (passphrase)))

;; CHAT protocol - implemented by STANDARD-CHAT
(defprotocol chat
    (chat () ()
      (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING."))
  (defgeneric name (object))
  (defgeneric messages (object))
  (defgeneric personas (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object))
  (defgeneric find-chat (name)))

;; PLAYER protocol - implemented by STANDARD-PLAYER
(defprotocol player
    (player () ()
      (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:EMAIL (optional) - a STRING.
:PASSWORD - a PASSWORD or a passphrase (a STRING).
"))
  (defgeneric name (object))
  (defgeneric email (object))
  (defgeneric password (object))
  (defgeneric personas (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object))
  (defgeneric find-player (name)))

;; PERSONA protocol - implemented by STANDARD-PERSONA
(defprotocol persona
    (persona () ()
      (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:PLAYER - a PLAYER.
:CHAT - a CHAT."))
  (defgeneric name (object))
  (defgeneric player (object))
  (defgeneric chat (object))
  (defgeneric find-persona (name)))

