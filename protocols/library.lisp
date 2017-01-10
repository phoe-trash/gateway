;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; library.lisp

(in-package #:gateway)

#|
Protocol class LIBRARY

Must be SYNCHRONIZED.
|#
(defprotocol library
    (library () ())
  (defgeneric players (library))
  (defgeneric lookup (key library))
  (defgeneric (setf lookup) (new-value key library)))
