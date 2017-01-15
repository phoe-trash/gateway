;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-persona.lisp

(in-package #:gateway)


(defclass standard-persona (persona)
  ((%name :accessor name
          :initarg :name
          :initform "Must provide name.")
   (%player :accessor player
            :initarg :player
            :initform nil)
   (%owner :accessor owner
           :initarg :owner
           :initform nil)
   (%chats :accessor chats
           :initform ())
   (%lock :accessor lock)))

(defconstructor (standard-persona)
  (unless (owner standard-persona)
    (setf (owner standard-persona) (player standard-persona)))
  (setf (lock standard-persona)
        (make-lock (format nil "Lock for persona ~S" (name standard-persona)))))
