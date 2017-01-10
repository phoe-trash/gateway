;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-socket.lisp

(in-package #:gateway)

(defclass standard-socket (stream-usocket)
  ((owner :accessor owner
          :initarg :owner
          :initform (error "Must provide an owner."))))
