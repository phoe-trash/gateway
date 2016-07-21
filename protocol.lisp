;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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













































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sexpable protocol
(defgeneric sexp (object)) ;; done

;; Password protocol
(defclass password () ())
(defgeneric make-password (passphrase)) ;; done
(defgeneric password-matches-p (password passphrase)) ;; done

;; Chatter protocol
(defclass chatter () ())
(defgeneric send-message (message recipient))

;; Message protocol
(defclass message () ())
(defgeneric sender (message))
(defgeneric (setf sender) (message))
(defgeneric recipient (message))
(defgeneric (setf recipient) (message))
(defgeneric date-of (message))
(defgeneric (setf date-of) (message))
(defgeneric contents (message))
(defgeneric (setf contents) (message))
(defgeneric msg (sender recipient contents))

;; Persona protocol
(defclass persona () ())
(defgeneric name (persona))
(defgeneric (setf name) (persona))
(defgeneric player (persona))
(defgeneric (setf player) (persona))
(defgeneric find-persona (name))
