;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; macros.lisp

(in-package #:gateway)

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let* ((sym-name (symbol-name name))
	 (protocol-predicate
	   (intern (concatenate 'string
				sym-name
				(if (find #\- sym-name) "-" "")
				(symbol-name '#:p))))
	 (predicate-docstring
	   (concatenate 'string
			"Returns T if the  " sym-name ".")))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)

       (let ((the-class (find-class ',name)))
	 (setf (documentation the-class 'type) "Gateway protocol class")
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "~S is a protocol class and thus can't be instantiated." ',name))))

       (defgeneric ,protocol-predicate (object)
	 (:method ((object t))
	   nil)
	 (:method ((object ,name))
	   t)
	 (:documentation ,predicate-docstring))

       ',name)))

(defmacro defprotoclass (name super-classes &optional slots &rest options)
  `(define-protocol-class ,name ,super-classes ,slots ,@options))

(defmacro defconstructor ((class . keys) &body body)
  `(defmethod initialize-instance :after ((,class ,class) &key ,@keys &allow-other-keys)
     ,@body))

(defmacro defprint (object &body body)
  `(defmethod print-object ((obj ,object) stream)
     ,@body))
