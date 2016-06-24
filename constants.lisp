;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defvar *password-iteration-count* 1000)
(defvar *password-key-length* 512)
(defvar *password-salt-length* 64)
(defvar *cache-lock* (make-lock))
(defvar *player-cache* nil)
(defvar *persona-cache* nil)
(defvar *socket-cache* nil)
