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
    (multiple-value-bind (fn foundp) (gethash (type-of condition) %condition-data%)
      (if foundp
          (funcall fn owner connection condition)
          (error "Condition ~A not found in hashtable." (type-of condition))))))
