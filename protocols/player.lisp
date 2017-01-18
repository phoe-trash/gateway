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
  (defgeneric password (player))
  (defgeneric (setf password) (new-value player))
  (defgeneric email (player))
  (defgeneric (setf email) (new-value player))
  (defgeneric personas (player))
  (defgeneric (setf personas) (new-value player))
  (defgeneric lock (player)))
