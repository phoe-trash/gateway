;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defaccessors sender recipient date-of contents
  id name player avatar gender species colors shard
  id name messages personas shard
  dimensions x-dimension y-dimension
  id username password email personas connection
  id name world-map jewel personas chats lock
  sexp)


(defgeneric find-persona (name))
(defgeneric location (object))
(defgeneric find-messages (chat &key sender recipient after-date before-date contents))
(defgeneric delete-message (message chat))
(defgeneric add-persona (persona chat))
(defgeneric delete-persona (persona chat))
(defgeneric make-password (passphrase))
(defgeneric password-matches-p (password passphrase))
(defgeneric object-at (world-map x y))
(defgeneric resize (world-map up left down right &key initial-element))
(defgeneric find-player (&key id username email))

(defgeneric output (object connection)) ;; done
(defgeneric input (connection &key safe-p)) ;; done <3
(defgeneric kill (connection)) ;; done





























;; chat.lisp
(defprotocol id (chat)) ;; done
(defprotocol name (chat)) ;; done
(defprotocol messages (chat)) ;; done
(defprotocol personas (chat)) ;; done
(defprotocol shard (chat)) ;; done

(defprotocol send-message (message chat)) ;; done
(defprotocol find-messages
    (chat &key sender recipient after-date before-date contents)) ;; done
(defprotocol delete-message (message chat)) ;; done

(defprotocol add-persona (persona chat)) ;; done
(defprotocol delete-persona (persona chat)) ;; done



;; world-map.lisp
(defprotocol dimensions (world-map)) ;; done
(defprotocol x-dimension (world-map)) ;; done
(defprotocol y-dimension (world-map)) ;; done
(defprotocol object-at (world-map x y)) ;; done
(defprotocol resize (world-map up left down right &key initial-element)) ;; done


;; player.lisp
(defprotocol id (player)) ;; done
(defprotocol username (player)) ;; done
(defprotocol password (player)) ;; done
(defprotocol email (player)) ;; done
(defprotocol personas (player)) ;; done
(defprotocol connection (player)) ;; done
(defprotocol send-message (message player)) ;; done
(defprotocol find-player (&key id username email)) ;; done


;;;; server protocol

;; shard.lisp
(defprotocol id (shard))
(defprotocol name (shard))
(defprotocol world-map (shard))
(defprotocol jewel (shard))
(defprotocol personas (shard))
(defprotocol chats (shard))
(defprotocol lock (shard))


;; connection.lisp
(defprotocol output (object connection)) ;; done
(defprotocol input (connection &key safe-p)) ;; done <3
(defprotocol kill (connection)) ;; done













































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SEXPABLE protocol

(defprotocol sexp (object))

;; MESSAGABLE protocol

(defprotocol send-message (message recipient))

;; MESSAGE protocol

(defclass message () ()
  (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:SENDER - the sender of the message.
:RECIPIENT - the recipient of the message.
:DATE - object of type DATE.
:CONTENTS - contents of the message (a STRING).
"))
(defprotocol sender (message))
(defprotocol recipient (message))
(defprotocol date (message))
(defprotocol contents (message))

;; PASSWORD protocol

(defclass password () ()
  (:documentation "Must be IMMUTABLE.

Constructor arguments:
:PASSPHRASE - a passphrase.
"))
(defprotocol password-matches-p (password passphrase))

;; PLAYER protocol

(defclass player () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:EMAIL (optional) - a STRING.
:PASSWORD - a PASSWORD or a passphrase (a STRING).
"))
(defprotocol name (player))
(defprotocol email (player))
(defprotocol personas (player))

;; PERSONA protocol

(defclass persona () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
:PLAYER - a PLAYER.
:CHAT - a CHAT.
"))
(defprotocol name (persona))
(defprotocol player (persona))
(defprotocol chat (persona))
(defprotocol find-persona (name))

;; DATE protocol

(defclass date () ()
  (:documentation "Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:TIMESTAMP (optional) - a timestamp, to be declared later.
"))
(defprotocol read-date (string))
(defprotocol print-object (date stream))
(defprotocol date= (date-1 date-2 &key fuzziness))
(defprotocol date< (date-1 date-2 &key fuzziness))
(defprotocol date> (date-1 date-2 &key fuzziness))

;; CHAT protocol

(defclass chat () ()
  (:documentation "Must be SEXPABLE and MESSAGABLE.

Constructor arguments:
:NAME - a STRING.
"))
(defprotocol name (chat))
(defprotocol messages (chat))
(defprotocol personas (chat))
