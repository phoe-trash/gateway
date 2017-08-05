;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass* gem ()
    ((owner (error "No owner provided.") :type (or jewel crown))
     (queue (make-synchro-queue) :type synchronized-queue)
     (worker nil :type thread)))
