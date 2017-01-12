;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; conditions.lisp

(in-package #:gateway)

(defvar %condition-data% (make-hash-table :test #'eq))

(define-condition gateway-condition () ())

;; TODO pull changes from DEFINE-GATEWAY-ERROR to DEFINE-GATEWAY-CONDITION

(defmacro define-gateway-condition
    (name slot-specs (owner-var connection-var condition-var &optional stream-var)
     report-data &body body)
  (let ((report (when (and stream-var report-data)
                  `((:report (lambda (,condition-var ,stream-var)
                               (format nil ,@report-data)))))))
    `(progn (define-condition ,name () ,slot-specs ,@report)
            (setf (gethash ',name %condition-data%)
                  (lambda (,owner-var ,connection-var ,condition-var)
                    ,@body)))))

(defun handle-gateway-condition (owner connection condition)
  (unless (typep condition 'gateway-error)
    (note "[!] Handling condition ~A.~%" (type-of condition))
    (multiple-value-bind (fn foundp) (gethash (type-of condition) %condition-data%)
      (if foundp
          (funcall fn owner connection condition)
          (error "Condition ~A not found in hashtable." (type-of condition))))))
