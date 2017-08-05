;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; name-taken.lisp

(in-package #:gateway)

#|
Error NAME-TAKEN

Should be signaled when the user provides an name which is already
registered to a player on the system.

Arguments:
* NAME: the taken name.
|#

(define-gateway-error name-taken
    ((name :reader name-taken-name
           :initarg :name
           :initform (error "Must provide name.")))
    (owner connection condition)
    (((name (name-taken-name condition)))
     ("The name ~A is already taken." name)
      (declare (ignore owner))
      (data-send connection `(:error :type :name-taken :name ,name))))
