;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass* shard ()
    ((world-map (error "No world map provided.") :type world-map)
     (jewel (error "No jewel provided.") :type jewel)
     (personas nil :type list)
     (chats nil :type list)
     (lock (make-lock))))
