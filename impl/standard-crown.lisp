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
   (%n-connections :accessor n-connections
                   :initform ())
   (%n-lock :accessor n-lock
            :initform (make-lock "Gateway - Crown N-lock"))
   (%n-listener :accessor n-listener)
   ;; E-CONNECTIONS
   (%e-connections :accessor e-connections
                   :initform ())
   (%e-lock :accessor e-lock
            :initform (make-lock "Gateway - Crown E-lock"))
   (%e-listener :accessor e-listener)
   ;; I-CONNECTIONS
   (%i-acceptor :accessor i-acceptor)
   (%i-connections :accessor i-connections
                   :initform ())
   (%i-lock :accessor i-lock
            :initform (make-lock "Gateway - Crown I-lock"))
   (%i-listener :accessor i-listener)))

(defun %crown-constructor-new (crown n-host n-port i-host i-port)
  (destructuring-bind (n-getter e-getter i-getter
                       n-pusher e-pusher i-pusher data-pusher)
      (%crown-constructor-lambdas crown)
    (let* ((library (make-instance 'standard-library))
           (queue (make-queue))
           (n-acceptor (%make-acceptor n-host n-port n-pusher))
           (i-acceptor (%make-acceptor i-host i-port i-pusher))
           (n-listener (%make-listener n-getter n-pusher data-pusher))
           (e-listener (%make-listener e-getter e-pusher data-pusher))
           (i-listener (%make-listener i-getter i-pusher data-pusher)))
      (setf (library crown) library
            (queue crown) queue
            (n-acceptor crown) n-acceptor
            (i-acceptor crown) i-acceptor
            (n-listener crown) n-listener
            (e-listener crown) e-listener
            (i-listener crown) i-listener))))

(defun %crown-constructor-lambdas (crown)
  (list
   (lambda () (with-lock-held ((n-lock crown))
                (n-connections crown)))
   (lambda () (with-lock-held ((e-lock crown))
                (e-connections crown)))
   (lambda () (with-lock-held ((i-lock crown))
                (i-connections crown)))
   (lambda (x) (with-lock-held ((n-lock crown))
                 (push x (n-connections crown))))
   (lambda (x) (with-lock-held ((e-lock crown))
                 (push x (e-connections crown))))
   (lambda (x) (with-lock-held ((i-lock crown))
                 (push x (i-connections crown))))
   (lambda (x) (push-queue x (queue crown)))))

(defconstructor (standard-crown new
                                (n-host "127.0.0.1") (n-port 0)
                                (i-host "127.0.0.1") (i-port 0))
  (if new
      (%crown-constructor-new standard-crown n-host n-port i-host i-port)
      (error "Constructing from existing data implemented yet.")))
