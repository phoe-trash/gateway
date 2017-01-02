;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-crown.lisp

(in-package #:gateway)

(defclass standard-crown (crown)
  ((%library :accessor library)
   (%queue :accessor queue)
   ;; N-CONNECTIONS
   (%n-acceptor :accessor n-acceptor)
   (%n-connections :accessor n-connections)
   (%n-lock :accessor n-lock
            :initform (make-lock "Gateway - Crown N-lock"))
   (%n-listener :accessor n-listener)
   ;; E-CONNECTIONS
   (%e-connections :accessor e-connections)
   (%e-lock :accessor e-lock
            :initform (make-lock "Gateway - Crown E-lock"))
   (%e-listener :accessor e-listener)
   ;; I-CONNECTIONS
   (%i-acceptor :accessor i-acceptor)
   (%i-connections :accessor i-connections)
   (%i-lock :accessor i-lock
            :initform (make-lock "Gateway - Crown I-lock"))
   (%i-listener :accessor i-listener)))

(defconstructor (standard-crown )
  )
