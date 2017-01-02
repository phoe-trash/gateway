;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; crown.lisp

(in-package #:gateway)

(defprotocol crown
    (crown () ())
  ;; DATA AND THREADS
  (defgeneric library (crown))
  (defgeneric queue (object))
  ;; N-CONNECTIONS
  (defgeneric n-acceptor (object))
  (defgeneric n-connections (object))
  (defgeneric n-lock (object))
  (defgeneric n-listener (object))
  ;; E-CONNECTIONS
  (defgeneric e-connections (object))
  (defgeneric e-lock (object))
  (defgeneric e-listener (object))
  ;; I-CONNECTIONS
  (defgeneric i-acceptor (object))
  (defgeneric i-connections (object))
  (defgeneric i-lock (object))
  (defgeneric i-listener (object))
  ;; METHODS
  (defgeneric kill (object))
  (defgeneric alivep (object)))


