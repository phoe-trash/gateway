;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; invalid-name.lisp

(in-package #:gateway)

#|
Error INVALID-NAME

Should be signaled when the user provides an name which is not
a valid name.

Arguments:
* NAME: the invalid name.
|#

(define-gateway-error invalid-name
    ((name :reader invalid-name-name
           :initarg :name
           :initform (error "Must provide name.")))
  (owner connection condition)
  (((name (invalid-name-name condition)))
   ("The provided name, ~S, was invalid." name)
   (declare (ignore owner))
   (data-send connection `(:error :type :invalid-name :name ,name))))
