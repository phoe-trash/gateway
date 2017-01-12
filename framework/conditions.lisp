;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; conditions.lisp

(in-package #:gateway)

(defvar %condition-data% (make-hash-table :test #'eq))

(define-condition gateway-condition () ())

(defmacro define-gateway-condition
    (name slot-specs (owner-var connection-var condition-var)
     (let-list report-data &body body))
  (let* ((vars (list owner-var connection-var condition-var))
         (main-lets (apply #'%condition-let-list let-list vars))
         (report (%condition-report condition-var let-list report-data)))
    `(progn (define-condition ,name (gateway-condition) ,slot-specs  ,@report)
            (setf (gethash ',name %condition-data%)
                  (lambda ,vars
                    (declare (ignorable ,@vars))
                    (let ,main-lets
                      (declare (ignorable ,@vars))
                      (note (cat (format nil "[!] Condition ~A: " ',name)
                                 (format nil ,@report-data))))
                    (let* ,main-lets
                      (declare (ignorable ,condition-var))
                      ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %condition-report (condition-var let-list report-data)
    (when report-data
      (with-gensyms (stream)
        `((:report (lambda (,condition-var ,stream)
                     (declare (ignorable ,condition-var))
                     (let ,let-list
                       (declare (ignorable ,@(mapcar #'car let-list)))
                       (format ,stream ,@report-data))))))))

  (defun %condition-let-list (let-list owner-var connection-var condition-var)
    (flet ((fn (var) (or (cadr (find var let-list :key #'car)) var)))
      (let* ((data `((,owner-var ,(fn owner-var))
                     (,connection-var ,(fn connection-var))
                     (,condition-var ,(fn condition-var))))
             (entries (set-difference let-list data :key #'first)))
        (append data entries)))))

(defun handle-gateway-condition (owner connection condition)
  (unless (typep condition 'gateway-condition)
    (note "[!] Handling condition ~A.~%" (type-of condition))
    (multiple-value-bind (fn foundp) (gethash (type-of condition) %condition-data%)
      (if foundp
          (funcall fn owner connection condition)
          (error "Condition ~A not found in hashtable." (type-of condition))))))
