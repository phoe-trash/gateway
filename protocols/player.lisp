;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; player.lisp

(in-package #:gateway)

#|
Protocol class PLAYER

Must be NAMED and SEXPABLE.
|#
(defprotocol player
    (player () ())
  (defgeneric username (player))
  (defgeneric password (player)))
