;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; constants.lisp

(in-package #:gateway)

(defvar *password-iteration-count* 1000)
(defvar *password-key-length* 512)
(defvar *password-salt-length* 64)

(defvar *persona-cache* (make-hash-table :test 'equal))
(defvar *player-cache* (make-hash-table :test 'equal))
(defvar *chat-cache* (make-hash-table :test 'equal))
