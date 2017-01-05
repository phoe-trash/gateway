;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; operations.lisp

(in-package #:gateway)

;;;; OPERATIONS

(defvar %operation-data% (make-hash-table))

(defun %execute (operation plist)
  (assert (proper-list-p plist))
  (assert (identity operation))
  (let* ((fn (gethash operation %operation-data%)))
    (if (null fn)
        (error "Operation ~S not found." operation)
        (progn (note "[G] Executing operation ~A.~%" operation)
               (funcall fn plist)))))

(defmacro defoperation (name keyword-list &body body)
  (let* ((gensym-sexp (gensym "OPERATION"))
         (args (list gensym-sexp))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',name %operation-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))
