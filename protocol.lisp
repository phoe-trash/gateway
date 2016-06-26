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
(defspecialization sender (message)) ;; done
(defspecialization recipient (message)) ;; done
(defspecialization date-of (message)) ;; done
(defspecialization contents (message)) ;; done
(defspecialization msg (sender recipient contents)) ;; done


;; persona.lisp
(defspecialization id (chat)) ;; done
(defspecialization name (persona)) ;; done
(defspecialization player (persona)) ;; done
(defspecialization avatar (persona)) ;; done
(defspecialization gender (persona)) ;; done
(defspecialization species (persona)) ;; done
(defspecialization colors (persona)) ;; done
(defspecialization shard (persona)) ;; done

(defspecialization find-persona (name)) ;; done
(defspecialization location (persona)) ;; done
(defspecialization send-message (message persona)) ;; done


;; chat.lisp
(defspecialization id (chat)) ;; done
(defspecialization name (chat)) ;; done
(defspecialization messages (chat)) ;; done
(defspecialization personas (chat)) ;; done
(defspecialization shard (chat)) ;; done

(defspecialization send-message (message chat)) ;; done
(defspecialization find-messages
    (chat &key sender recipient after-date before-date contents)) ;; done
(defspecialization delete-message (message chat)) ;; done

(defspecialization add-persona (persona chat)) ;; done
(defspecialization delete-persona (persona chat)) ;; done


;; password.lisp
(defspecialization make-password (passphrase)) ;; done
(defspecialization password-matches-p (password passphrase)) ;; done


;; world-map.lisp
(defspecialization dimensions (world-map)) ;; done
(defspecialization x-dimension (world-map)) ;; done
(defspecialization y-dimension (world-map)) ;; done
(defspecialization object-at (world-map x y)) ;; done
(defspecialization resize (world-map up left down right &key initial-element)) ;; done


;; player.lisp
(defspecialization id (player)) ;; done
(defspecialization username (player)) ;; done
(defspecialization password (player)) ;; done
(defspecialization email (player)) ;; done
(defspecialization personas (player)) ;; done
(defspecialization connection (player)) ;; done
(defspecialization send-message (message player)) ;; done
(defspecialization find-player (&key id username email)) ;; done


;;;; server protocol

;; shard.lisp
(defspecialization id (shard))
(defspecialization world-map (shard))
(defspecialization jewel (shard))
(defspecialization personas (shard))
(defspecialization chats (shard))
(defspecialization lock (shard))


;; connection.lisp
(defspecialization output (object connection)) ;; done
(defspecialization input (connection &key safe-p)) ;; done <3
(defspecialization kill (connection)) ;; done


;; get-sexp.lisp
(defspecialization get-sexp (object))
