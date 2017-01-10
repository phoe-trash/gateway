;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; conditions.lisp

(in-package #:gateway)

(defvar %condition-data% (make-hash-table :test #'eq))

(define-condition gateway-condition () ())

(defmacro define-gateway-condition (name slot-specs (owner-var connection-var condition-var)
                                    &body body)
  `(progn (define-condition ,name () ,slot-specs)
          (setf (gethash ',name %condition-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))

(defun handle-gateway-condition (owner connection condition)
  (%handle-gateway-condition owner connection condition %condition-data%))



(defvar %error-data% (make-hash-table :test #'eq))

(define-condition gateway-error (gateway-condition error) ())

(defmacro define-gateway-error (name slot-specs (owner-var connection-var condition-var)
                                &body body)
  `(progn (define-condition ,name (gateway-condition error) ,slot-specs)
          (setf (gethash ',name %error-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))

(defun handle-gateway-error (owner connection error)
  (%handle-gateway-condition owner connection error %error-data%))



(defun %handle-gateway-condition (owner connection error hashtable)
  (let ((fn (gethash (type-of error) hashtable)))
    (funcall fn owner connection error)))
