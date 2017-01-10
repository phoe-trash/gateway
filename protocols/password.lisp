;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; password.lisp

(in-package #:gateway)

#|
Protocol class PASSWORD

Must be SEXPABLE and IMMUTABLE.

Constructor arguments:
:PASSPHRASE - a passphrase.
|#

(defprotocol password
    (password () ()
      (:documentation ""))
  (defgeneric password-matches-p (password passphrase))
  (defgeneric make-password (passphrase)))
