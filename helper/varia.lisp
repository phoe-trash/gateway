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

;; defgeneric - just the generic method with generic lambda list
;; defaccessor - (defgeneric fun-name (object))
;; defaccessors - mapcar #'%defaccessor fun-names
;; defspecialization - warn if function unbound/not generic, stub warning method

(defmacro defaccessors (&rest fun-names)
  (cons 'progn
	(mapcar #'%defaccessor (remove-duplicates fun-names))))

(defmacro defaccessor (fun-name)
  (%defaccessor fun-name))

(defun %defaccessor (fun-name)
  `(defgeneric ,fun-name (object)))

(defmacro defspecialization (fun-name lambda-list)
  (when (fboundp fun-name)
    (check-type (fdefinition fun-name) generic-function)) 
  (flet ((get-specializers ()
	   (mapcar #'method-specializers (generic-function-methods (fdefinition fun-name))))
	 (length=1 (x) (= 1 (length x))))
    (if (or (not (fboundp fun-name))
	    (not (every #'length=1 (get-specializers))))
	(if (length=1 lambda-list)
	    (%defspecialization-accessor fun-name lambda-list)
	    (%defspecialization-method fun-name lambda-list)) 
	`(cond 
	   ((not (typep (function ,fun-name) 'generic-function))
	    (error "~S is already fbound as a non-generic function." ',fun-name))
	   ((member t (mapcar #'class-name (flatten ',(get-specializers))))
	    nil)
	   (t
	    ,(%defspecialization-accessor fun-name lambda-list))))))

(defun %defspecialization-method (fun-name lambda-list) 
  `(defmethod ,fun-name ,lambda-list 
     ,(%ignorables lambda-list)
     (todo ,(format nil "~S default case called (method not implemented?)."
		    (list* fun-name lambda-list)))))

(defun %defspecialization-accessor (fun-name lambda-list)
  `(defmethod ,fun-name ,lambda-list
     ,(%ignorables lambda-list)
     (todo ,(format nil "~S default case called (accessor not implemented?)."
		    (list* fun-name lambda-list)))))

(defun %ignorables (lambda-list)
  (multiple-value-bind (args opts rest keys auxes)
      (parse-ordinary-lambda-list lambda-list)
    (let ((all-vars (remove nil (append args (mapcar #'first opts) (list rest)
					(mapcar (compose #'second #'first) keys)
					(mapcar #'first auxes)))))
      `(declare (ignorable ,@all-vars)))))
