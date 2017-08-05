;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defmacro defconstructor ((class . keys) &body body)
  `(defmethod initialize-instance :after ((,class ,class) &key ,@keys &allow-other-keys)
     ,@body))

(defmacro defprotocol (&rest rest)
  (declare (ignore rest)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defaccessors (&rest fun-names)
    (cons 'progn (mapcar #'%defaccessor (remove-duplicates fun-names))))

  (defmacro defaccessor (fun-name)
    (%defaccessor fun-name))

  (defun %defaccessor (fun-name)
    `(defgeneric ,fun-name (object)))

  (defmacro defspecialization (fun-name lambda-list &body body)
    `(progn
       (%ensure-specialization ',fun-name)
       (defmethod ,fun-name ,lambda-list ,@body)))

  (defun %ensure-specialization (fun-name)
    (cond ((not (fboundp fun-name))
	   (error "Symbol ~S is unbound as a function." fun-name))
	  ((not (typep (fdefinition fun-name) 'generic-function))
	   (error "~S is not a generic function." fun-name))
	  (t
	   (fdefinition fun-name)))))
