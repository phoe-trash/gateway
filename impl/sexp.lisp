;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sexp.lisp

(in-package #:gateway)

;; SEXP
(defmethod sexp (object)
  (format t "[!] SEXP: no method for ~S of type ~S. Bug?~%" object (type-of object))
  object)

(defmethod sexp ((object integer))
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
    (funcall fn sexp parent)))
