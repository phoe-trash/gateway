;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; macros.lisp

(in-package #:gateway)

(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let* ((sym-name (symbol-name name))
	 (protocol-predicate
	   (intern (concatenate 'string sym-name
				(if (find #\- sym-name) "-" "") (symbol-name '#:p))))
	 (predicate-docstring
	   (concatenate 'string "Returns T if object is of class " sym-name
			", otherwise returns NIL.")))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)
       (let ((the-class (find-class ',name)))
	 (setf (documentation the-class 'type) "Gateway protocol class")
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "~S is a protocol class and thus can't be instantiated." ',name))))
       (defgeneric ,protocol-predicate (object)
	 (:method ((object t)) nil)
	 (:method ((object ,name)) t)
	 (:documentation ,predicate-docstring))
       ',name)))

(defmacro defconstructor ((class . keys) &body body)
  `(defmethod initialize-instance :after ((,class ,class) &key ,@keys &allow-other-keys)
     ,@body))

(defmacro defprint (object &body body)
  `(defmethod print-object ((obj ,object) stream)
     ,@body))

(defmacro defprotocol (protocol-name
		       (&optional class-name class-args class-slots &body class-options)
		       &body body)
  (declare (ignore protocol-name))
  `(progn
     ,(when class-name
	`(define-protocol-class ,class-name ,class-args ,class-slots ,@class-options))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *config-vars* nil)
  (defvar *cache-vars* nil)
  (defvar *cache-list* (make-hash-table)) 
  (defmacro defconfig (var val &key cache doc)
    `(progn (pushnew (list ',var ',val) *config-vars* :test #'equal) 
	    (defvar ,var ,val ,doc)
	    ,@(when cache
		`((pushnew (list ',cache ',var) *cache-vars*)
		  (setf (gethash ,cache *cache-list*) ,var)))))
  (defmacro with-clean-config (&body body)
    `(let ((*config-vars* nil) ,@*config-vars*)
       ;;TODO: use PROGV for dynamic binding
       (declare (ignorable *config-vars* ,@#1=(mapcar #'first *config-vars*)))
       ;; reconstruct *CONFIG-VARS*
       #|(map nil (lambda (x y) (pushnew (list x y) *config-vars* :test #'equal))
       ',#1# (mapcar #'symbol-value ',#1#))|#
       ;; reconstruct *CACHE-LIST*
       (mapc (lambda (x) (setf (gethash (first x) *cache-list*) (second x)))
	     (list ,@(mapcar (lambda (x) `(list ',(first x) ,(second x))) *cache-vars*)))
       ,@body)))

(defmacro with-connections (connections &body body)
  `(let* ,connections
     (unwind-protect
	  ,@body 
       (mapcar #'kill (list ,@(mapcar #'first connections))))))
