;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sexp.lisp

(in-package #:gateway)

;; SEXP
(defmethod sexp (object)
  (note "[!] SEXP: no method for ~S of type ~S. Bug?~%" object (type-of object))
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

(defmacro defunsexp (name keyword-list owner &body body)
  (let* ((gensym-sexp (gensym "SEXP"))
         (gensym-owner (gensym "OWNER"))
         (owner-list (if (car owner) (list (car owner)) (list gensym-owner)))
         (args (cons gensym-sexp owner-list))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash (string ',name) %unsexp-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,(when (car owner)
                  `(assert ,(car owner) () "Owner must be provided."))
               ,@body)))))
