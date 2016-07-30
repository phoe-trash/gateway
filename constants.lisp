;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; constants.lisp

(in-package #:gateway)

(defconfig *password-iteration-count* 1000)
(defconfig *password-key-length* 512)
(defconfig *password-salt-length* 64)

(defconfig *cache-list* (make-hash-table))
(defconfig *cache-lock* (make-lock))
(defconfig *persona-cache* (make-hash-table :test 'equal) :cache :persona)
(defconfig *player-cache* (make-hash-table :test 'equal) :cache :player)
(defconfig *chat-cache* (make-hash-table :test 'equal) :cache :chat)
(defconfig *connection-cache* (make-hash-table) :cache :connection)
