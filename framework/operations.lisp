;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; operations.lisp

(in-package #:gateway)

;;;; OPERATIONS

(defvar %operation-data% (make-hash-table :test #'eq))

(defun %execute-operation (operation plist)
  (assert (proper-list-p plist))
  (assert (identity operation))
  (let* ((fn (gethash operation %operation-data%)))
    (if fn
        (funcall fn plist)
        (error "Operation ~S not found." operation))))

(defmacro defoperation (name keyword-list &body body)
  (let* ((gensym-sexp (gensym "OPERATION"))
         (args (list gensym-sexp))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',name %operation-data%)
           (lambda ,args
             (note "[G] Executing operation ~A.~%" ,name)
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))
