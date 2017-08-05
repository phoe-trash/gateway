;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass* jewel ()
    ((shards nil :type list)
     (gems nil :type list)
     (queue (make-synchro-queue) :type synchronized-queue)
     (io-int :type connection)
     (io-db :type connection)))
