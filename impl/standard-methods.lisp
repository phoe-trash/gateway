;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-methods.lisp

(in-package #:gateway)

(defmethod sexp (object)
  (print (format nil "[!] No idea how to SEXP ~S. Bug?" object)))

(defmethod sexp ((object list))
  (mapcar #'sexp object))

(defmethod sexp ((object integer))
  object)

(defmethod sexp ((object string))
  object)

(defmethod sexp ((object symbol))
  object)
