;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; errors.lisp

(in-package #:gateway)

(defvar %error-data% (make-hash-table :test #'eq))

(define-condition gateway-error (gateway-condition error) ())

(defmacro define-gateway-error (name slot-specs (owner-var connection-var condition-var)
                                &body body)
  `(progn (define-condition ,name (gateway-error) ,slot-specs)
          (setf (gethash ',name %error-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))

(defun handle-gateway-error (owner connection error)
  (check-type error gateway-error)
  (note "[!] Handling error ~A.~%" (type-of error))
  (multiple-value-bind (fn foundp) (gethash (type-of error) %error-data%)
    (if foundp
        (funcall fn owner connection error)
        (error "Error ~A not found in hashtable." (type-of error)))))
