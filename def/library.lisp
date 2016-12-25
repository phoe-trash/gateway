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
  (defgeneric lookup (library key))
  (defgeneric (setf lookup) (library new-value key)))
