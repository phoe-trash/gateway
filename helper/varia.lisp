;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defmacro defprint (object &body body)
  `(defmethod print-object ((obj ,object) stream)
     ,@body))

(defmacro defconstructor ((class . keys) &body body)
  `(defmethod initialize-instance :after ((,class ,class) &key ,@keys &allow-other-keys)
     ,@body))
