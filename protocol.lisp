;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocol.lisp

(in-package #:gateway)

;; SEXPABLE protocol

(defgeneric sexp (object))

;; MESSAGABLE protocol

(defgeneric send-message (message recipient))

;; MESSAGE protocol

(defprotoclass message () () 
  (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:SENDER - the sender of the message.
:RECIPIENT - the recipient of the message.
:DATE - object of type DATE.
:CONTENTS - contents of the message (a STRING).
"))
(defgeneric sender (object))
(defgeneric recipient (object))
(defgeneric date (object))
(defgeneric contents (object))

;; PASSWORD protocol

(defprotoclass password () ()
  (:documentation "Must be IMMUTABLE.

Constructor arguments:
:PASSPHRASE - a passphrase.
"))
(defgeneric password-matches-p (password passphrase))

;; PLAYER protocol

(defprotoclass player () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:EMAIL (optional) - a STRING.
:PASSWORD - a PASSWORD or a passphrase (a STRING).
"))
(defgeneric name (object))
(defgeneric email (object))
(defgeneric personas (object))

;; PERSONA protocol

(defprotoclass persona () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:PLAYER - a PLAYER.
:CHAT - a CHAT.
"))
(defgeneric name (object))
(defgeneric player (object))
(defgeneric chat (object))
(defgeneric find-persona (name))

;; DATE protocol

(defprotoclass date () ()
  (:documentation "Must be SEXPABLE and IMMUTABLE.
"))
(defgeneric parse-date (string))
(defgeneric date= (date-1 date-2 &key unit))
(defgeneric date/= (date-1 date-2 &key unit))
(defgeneric date< (date-1 date-2))
(defgeneric date<= (date-1 date-2 &key unit))
(defgeneric date> (date-1 date-2))
(defgeneric date>= (date-1 date-2 &key unit))
(defgeneric date-min (date-1 &rest dates))
(defgeneric date-max (date-1 &rest dates))

;; CHAT protocol

(defprotoclass chat () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
"))
(defgeneric name (object))
(defgeneric messages (object))
(defgeneric personas (object))
