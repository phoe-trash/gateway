;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-crown.lisp

(in-package #:gateway)

(defclass standard-crown (crown)
  (;; LIBRARY
   (%library :accessor library
             :initform (make-hash-table :test #'equal))
   (%library-lock :accessor library-lock)
   ;; EVENT QUEUE
   (%event-queue :accessor event-queue
                 :initform (make-synchro-queue))
   (%gems :accessor gems :initform nil)
   ;; N-CONNECTIONS
   (%n-acceptor :accessor n-acceptor)
   (%n-connections :accessor n-connections :initform nil)
   (%n-lock :accessor n-lock)
   (%n-listener :accessor n-listener) ;; TODO
   ;; E-CONNECTIONS
   (%e-connections :accessor e-connections :initform nil)
   (%e-lock :accessor e-lock)
   (%e-listener :accessor e-listener) ;; TODO
   ;; I-CONNECTIONS
   (%i-acceptor :accessor i-acceptor)
   (%i-connections :accessor i-connections :initform nil)
   (%i-lock :accessor i-lock)
   (%i-listener :accessor i-listener))) ;; TODO

(defconstructor (standard-crown (e-host "127.0.0.1") (e-port 65001)
                                (i-host "127.0.0.1") (i-port 65002))
  (check-type e-host string)
  (check-type e-port (unsigned-byte 16))
  (check-type i-host string)
  (check-type i-port (unsigned-byte 16))
  (setf
   ;; LOCKS
   (library-lock standard-crown)
   (make-lock (format nil "Gateway - library-lock for crown on ~A:~S" e-host e-port))
   (n-lock standard-crown)
   (make-lock (format nil "Gateway - N-lock for crown on ~A:~S" e-host e-port))
   (e-lock standard-crown)
   (make-lock (format nil "Gateway - E-lock for crown on ~A:~S" e-host e-port))
   (i-lock standard-crown)
   (make-lock (format nil "Gateway - I-lock for crown on ~A:~S" e-host e-port))
   ;; ACCEPTORS
   (n-acceptor standard-crown)
   (make-instance 'standard-acceptor :hort e-host :port e-port :crown standard-crown :type :n)
   (i-acceptor standard-crown)
   (make-instance 'standard-acceptor :hort i-host :port i-port :crown standard-crown :type :i)
   ;; TODO: connections
   ;; TODO: listeners
   ))
