;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sexp.lisp

(in-package #:gateway)

;; SEXP
(defmethod sexp (object)
  (format t "[!] SEXP: no method for ~S of type ~S. Bug?~%" object (type-of object))
  object)

(defmethod sexp ((object number))
  object)

(defmethod sexp ((object string))
  object)

(defmethod sexp ((object symbol))
  object)

(defmethod sexp ((object list))
  (mapcar #'sexp object))

;;;; UNSEXP
(defvar %unsexp-data% (make-hash-table :test #'equalp))

(defun %unsexp (sexp &optional parent)
  (assert (proper-list-p sexp))
  (assert (identity sexp))
  (let* ((data-word (string (first sexp)))
         (fn (gethash data-word %unsexp-data%)))
    (funcall fn (cdr sexp) parent)))

(defmacro defunsexp (name keyword-list parent &body body)
  (let* ((gensym-sexp (gensym "SEXP"))
         (gensym-parent (gensym "PARENT"))
         (parent-list (if (car parent) (list (car parent)) (list gensym-parent)))
         (args (cons gensym-sexp parent-list))
         (let-list (%defunsexp-let-list keyword-list gensym-sexp)))
    `(setf (gethash (string ',name) %unsexp-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))

(defun %defunsexp-let-list (keyword-list gensym)
  (flet ((fn (x) `(,(first x) (data-getf ,gensym ',(second x)))))
    (mapcar #'fn keyword-list)))
