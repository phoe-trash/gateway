;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocol.lisp

(in-package #:gateway)

;; TODO: refactor crown subfunctions
;; TODO: rethink connections and their locks
;; TODO: lock refactor

;; SEXPABLE protocol
(defprotocol sexpable () 
  (defgeneric sexp (object))
  (defun parse (sexp &optional parent)
    (%parse sexp parent)))

;; MESSAGABLE protocol
(defprotocol messagable ()
  (defgeneric send-message (message recipient)))

;; IDENTIFIABLE protocol
(defprotocol identifiable ()
  (defun identify (type key)
    (%identify type key)))

;; CACHE protocol
(defprotocol cache ()
  (defun cache (type key)
    (%cache type key))
  (defun (setf cache) (new-value type key)
    (setf (%cache type key) new-value)))

;; DATE protocol - implemented by STANDARD-DATE
(defprotocol date
    (date () ()
      (:documentation "Must be SEXPABLE and IMMUTABLE.

The :UNIT argument must accepts arguments 
:YEAR, :MONTH, :DAY, :HOUR, :MINUTE, :SECOND, :NANOSECOND."))
  (defgeneric parse-date (string))
  (defgeneric date= (date-1 date-2 &key unit))
  (defgeneric date/= (date-1 date-2 &key unit))
  (defgeneric date< (date-1 date-2))
  (defgeneric date<= (date-1 date-2 &key unit))
  (defgeneric date> (date-1 date-2))
  (defgeneric date>= (date-1 date-2 &key unit))
  (defgeneric date-min (&rest dates))
  (defgeneric date-max (&rest dates))
  (defgeneric now ()))

;; PASSWORD protocol - implemented by STANDARD-PASSWORD
(defprotocol password
    (password () ()
      (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:PASSPHRASE - a passphrase."))
  (defgeneric password-matches-p (password passphrase))
  (defgeneric make-password (passphrase)))

;; CONNECTION protocol - implemented by STANDARD-CONNECTION
(defprotocol connection
    (connection () ()
      (:documentation "Constructor arguments:

:TYPE - one of :LISTEN, :ACCEPT, :CLIENT.
:HOST - hostname.
:PORT - port."))
  (defgeneric send (connection object))
  (defgeneric receive (connection &key))
  (defgeneric readyp (connection))
  (defgeneric kill (object))
  (defgeneric alivep (object)))

;; PLAYER protocol - implemented by STANDARD-PLAYER
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

;; PERSONA protocol - implemented by STANDARD-PERSONA
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
  (defgeneric msg (sender recipient contents &key date))
  (defgeneric message= (message-1 message-2)))

;; CHAT protocol - implemented by STANDARD-CHAT
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

;; SHARD protocol - no impl ;_;
(defprotocol shard
    (shard () ())

  (defgeneric players (object))
  (defgeneric chats (object))
  (defgeneric add-persona (persona object))
  (defgeneric delete-persona (persona object)))

;; CROWN protocol - no impl ;_;
(defprotocol crown
    (crown () ())
  ;; LIBRARY
  (defgeneric lookup (key))
  (defgeneric (setf lookup) (new-value key))
  ;; EVENT QUEUE
  (defgeneric event-queue (object))
  (defgeneric gems (object))
  ;; N-CONNECTIONS
  (defgeneric n-acceptor (object))
  (defgeneric n-connections (object))
  (defgeneric n-lock (object))
  (defgeneric n-listener (object)) 
  ;; E-CONNECTIONS
  (defgeneric e-connections (object))
  (defgeneric e-lock (object))
  (defgeneric e-listener (object)) 
  ;; I-CONNECTIONS
  (defgeneric i-acceptor (object))
  (defgeneric i-connections (object))
  (defgeneric i-lock (object))
  (defgeneric i-listener (object))
  ;; METHODS
  (defgeneric kill (object))
  (defgeneric alivep (object)))

;; ACCEPTOR protocol - no impl ;_;
(defprotocol acceptor
    (acceptor () ())
  (defgeneric socket (object))
  (defgeneric thread (object))
  (defgeneric owner (object))
  (defgeneric kill (object))
  (defgeneric alivep (object)))

;; LISTENER protocol - implemented by STANDARD-LISTENER
(defprotocol listener
    (listener () ())
  (defgeneric owner (object)))

;; LISTENER protocol - implemented by STANDARD-LISTENER
(defprotocol gem
    (gem () ()))

;; JEWEL protocol - no impl ;_;
(defprotocol jewel
    (jewel () ()))

;; SHARD
;; GEM
;; JEWEL
;; CROWN
