;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defun value (key alist)
  (cdr (assoc key alist)))

(defun key (value alist)
  (car (rassoc value alist)))

(defun call-n-times (function n arg)
  (let ((result arg))
    (dotimes (i n result)
      (setf result (funcall function result)))))

(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun only-one-p (&rest args)
  (let ((args (remove-if #'null args)))
    (= 1 (length args))))

(defun condition-key (condition)
  (intern (string (type-of condition)) (find-package :keyword)))

(defmacro defprint (object &body body)
  `(defmethod print-object ((obj ,object) stream)
     ,@body))

(defmacro both (pred expr-1 expr-2)
  `(and (funcall ,pred ,expr-1)
	(funcall ,pred ,expr-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ID ASSIGNMENT

(defmethod assign-id (object)
  (let ((id (iter (incf *id-counter*)
	      (until `(both #'null ,@(multiple-value-list
				      (gethash *id-counter* *id-hash-table*))))
	      (finally (return *id-counter*)))))
    (setf (gethash id *id-hash-table*) object)
    id))
