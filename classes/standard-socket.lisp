;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-socket.lisp

(in-package #:gateway/impl)

(defclass standard-socket (stream-usocket)
  ((owner :accessor owner
          :initarg :owner
          :initform (error "Must provide an owner."))))
