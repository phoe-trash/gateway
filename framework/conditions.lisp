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
  (unless (typep condition 'gateway-error)
    (note "[!] Handling condition ~A.~%" (type-of condition))
    (%handle-gateway-condition owner connection condition %condition-data%)))



(defvar %error-data% (make-hash-table :test #'eq))

(define-condition gateway-error (gateway-condition error) ())

(defmacro define-gateway-error (name slot-specs (owner-var connection-var condition-var)
                                &body body)
  `(progn (define-condition ,name (gateway-error) ,slot-specs)
          (setf (gethash ',name %error-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))

(defun handle-gateway-error (owner connection error)
  (note "[!] Handling error ~A.~%" (type-of error))
  (%handle-gateway-condition owner connection error %error-data%))

(defun %handle-gateway-condition (owner connection condition hashtable)
  (let ((fn (gethash (type-of condition) hashtable)))
    (if fn
        (funcall fn owner connection condition)
        (error "Condition ~A not found in hashtable." (type-of condition)))))
