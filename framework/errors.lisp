;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; errors.lisp

(in-package #:gateway)

(defvar %error-data% (make-hash-table :test #'eq))

(define-condition gateway-error (gateway-condition error) ())

(defmacro define-gateway-error
    (name slot-specs (owner-var connection-var condition-var)
     (let-list report-data &body body))
  (let* ((vars (list owner-var connection-var condition-var))
         (main-lets (apply #'%error-let-list let-list vars))
         (report (%error-report condition-var let-list report-data)))
    `(progn (define-condition ,name (gateway-error) ,slot-specs  ,@report)
            (setf (gethash ',name %error-data%)
                  (lambda ,vars
                    (declare (ignorable ,@vars))
                    (let ,main-lets
                      (declare (ignorable ,@vars ,@(mapcar #'car let-list)))
                      (note (cat (format nil "[!] Error ~A: " ',name)
                                 (format nil ,@report-data))))
                    (let* ,main-lets
                      (declare (ignorable ,condition-var))
                      ,@body))))))

;; TODO: fix ignorables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %error-report (condition-var let-list report-data)
    (when report-data
      (with-gensyms (stream)
        `((:report (lambda (,condition-var ,stream)
                     (declare (ignorable ,condition-var))
                     (let ,let-list
                       (declare (ignorable ,@(mapcar #'car let-list)))
                       (format ,stream ,@report-data))))))))

  (defun %error-let-list (let-list owner-var connection-var condition-var)
    (flet ((fn (var) (or (cadr (find var let-list :key #'car)) var)))
      (let* ((data `((,owner-var ,(fn owner-var))
                     (,connection-var ,(fn connection-var))
                     (,condition-var ,(fn condition-var))))
             (entries (set-difference let-list data :key #'first)))
        (append data entries)))))

(defun handle-gateway-error (owner connection error)
  (check-type error gateway-error)
  (note "[!] Handling error ~A.~%" (type-of error))
  (multiple-value-bind (fn foundp) (gethash (type-of error) %error-data%)
    (if foundp
        (funcall fn owner connection error)
        (error "Error ~A not found in hashtable." (type-of error)))))
