
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-jewel.lisp

(defclass standard-jewel (jewel)
  (;; LIBRARY
   (%library :accessor library
             :initform (make-instance 'standard-library))
   ;; EVENT-QUEUE
   (%event-queue :accessor event-queue
                 :initform (make-synchro-queue))
   (%gems :accessor gems
          :initform nil)
   (%connection)
   (%connector)
   (%shards)
   (%shards-lock)))
