;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-crown.lisp

(in-package #:gateway)

(defclass standard-crown (crown)
  ((%library :accessor library)
   (%queue :accessor queue)
   (%n-acceptor :accessor n-acceptor)
   (%n-connections :accessor n-connections :initform ())
   (%n-lock :accessor n-lock :initform (%crown-lock "N"))
   (%n-listener :accessor n-listener)
   (%e-connections :accessor e-connections :initform ())
   (%e-lock :accessor e-lock :initform (%crown-lock "E"))
   (%e-listener :accessor e-listener)
   (%i-acceptor :accessor i-acceptor)
   (%i-connections :accessor i-connections :initform ())
   (%i-lock :accessor i-lock :initform (%crown-lock "I"))
   (%i-listener :accessor i-listener)))

(defun %crown-lock (string)
  (make-lock (format nil "Gateway - Crown ~A-lock" string)))

(defconstructor (standard-crown new
                                (n-host "127.0.0.1") (n-port 0)
                                (i-host "127.0.0.1") (i-port 0))
  (if new
      (%crown-constructor-new standard-crown n-host n-port i-host i-port)
      (error "Constructing from existing data not implemented yet.")))

(defun %crown-constructor-new (crown n-host n-port i-host i-port)
  (setf (values (library crown) (queue crown)
                (n-acceptor crown) (i-acceptor crown)
                (n-listener crown) (e-listener crown) (i-listener crown))
        (%crown-constructor-objects crown n-host n-port i-host i-port)))

(defun %crown-constructor-objects (crown n-host n-port i-host i-port)
  (destructuring-bind (n-getter e-getter i-getter
                       n-pusher e-pusher i-pusher data-pusher)
      (%crown-constructor-lambdas crown)
    (values (make-instance 'standard-library)
            (make-queue)
            (%make-acceptor n-host n-port n-pusher)
            (%make-acceptor i-host i-port i-pusher)
            (%make-listener n-getter n-pusher data-pusher)
            (%make-listener e-getter e-pusher data-pusher)
            (%make-listener i-getter i-pusher data-pusher))))

(defun %crown-constructor-lambdas (crown)
  (list
   (lambda () (with-lock-held ((n-lock crown)) (n-connections crown)))
   (lambda () (with-lock-held ((e-lock crown)) (e-connections crown)))
   (lambda () (with-lock-held ((i-lock crown)) (i-connections crown)))
   (lambda (x) (with-lock-held ((n-lock crown)) (push x (n-connections crown))))
   (lambda (x) (with-lock-held ((e-lock crown)) (push x (e-connections crown))))
   (lambda (x) (with-lock-held ((i-lock crown)) (push x (i-connections crown))))
   (lambda (x) (push-queue x (queue crown)))))
