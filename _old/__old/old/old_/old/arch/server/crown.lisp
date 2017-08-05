;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass* crown ()
    ((gems nil :type list)
     (queue (make-synchro-queue) :type synchronized-queue)
     (shard->jewel (make-hash-table :test #'eq) :type hash-table)
     (chats nil :type list)
     (io-exts nil :type list)
     (io-ints nil :type list)
     (io-db :type connection)))
