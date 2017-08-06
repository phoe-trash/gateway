;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/password.lisp

(in-package :gateway/protocols)

(define-protocol password
    (:description "This protocol describes password objects, which are ~
immutable and contain a hashed representation of a passphrase string. The only ~
functionality requested from a password is the PASSWORD-MATCHES-P function."
     :tags (:password)
     :dependencies (serializable)
     :export t)
  (:class password (serializable) ())
  "A password object."
  (:function password-matches-p ((password password) (pasphrase string))
             :generalized-boolean)
  "Returns true iff a passphrase matches a given password object and false
otherwise.")
