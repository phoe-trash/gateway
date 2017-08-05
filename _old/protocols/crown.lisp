;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; crown.lisp

(in-package #:gateway)

#|
Protocol class CROWN

Must be KILLABLE.
|#

(defprotocol crown
    (crown () ())
  ;; DATA
  (defgeneric library (crown keyword))
  (defgeneric inactive-players (crown))
  (defgeneric (setf inactive-players) (new-value crown))
  (defgeneric inactive-players-lock (crown))
  ;; THREADS
  (defgeneric queue (object))
  (defgeneric timer (object))
  (defgeneric gems (object))
  (defgeneric (setf gems) (new-value object))
  (defgeneric operations (object))
  (defgeneric (setf operations) (new-value object))
  ;; N-CONNECTIONS
  (defgeneric n-acceptor (object))
  (defgeneric n-connections (object))
  (defgeneric (setf n-connections) (new-value object))
  (defgeneric n-lock (object))
  (defgeneric n-listener (object))
  ;; E-CONNECTIONS
  (defgeneric e-connections (object))
  (defgeneric (setf e-connections) (new-value object))
  (defgeneric e-lock (object))
  (defgeneric e-listener (object))
  ;; I-CONNECTIONS
  (defgeneric i-acceptor (object))
  (defgeneric i-connections (object))
  (defgeneric (setf i-connections) (new-value object))
  (defgeneric i-lock (object))
  (defgeneric i-listener (object))
  ;; METHODS
  (defgeneric kill (object))
  (defgeneric alivep (object)))
