;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; conditions.lisp

(in-package #:gateway)

(defvar %condition-data% (make-hash-table :test #'eq))

(define-condition gateway-condition () ())

(defmacro define-gateway-condition (name slot-specs (owner-var connection-var condition-var)
                                    &body body)
  `(progn (define-condition ,name () ,slot-specs)
          (setf (gethash ',name %condition-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))



(defvar %error-data% (make-hash-table :test #'eq))

(define-condition gateway-error (gateway-condition error) ())

(defmacro define-gateway-error (name slot-specs (owner-var connection-var condition-var)
                                &body body)
  `(progn (define-condition ,name (gateway-condition error) ,slot-specs)
          (setf (gethash ',name %error-data%)
                (lambda (,owner-var ,connection-var ,condition-var)
                  ,@body))))

#|
Error ALREADY-LOGGED-IN

Should be signaled when the user tries to perform a LOGIN command while
already being logged in.

Arguments:
* AUTH: the NAMED object representing whomever the user is already logged
in as.
|#
(define-gateway-error already-logged-in
    ((auth :accessor already-logged-in-auth
           :initarg :auth
           :initform (error "Must provide previous auth.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((name (name (already-logged-in-auth condition))))
    (data-send connection `(:error :type :already-logged-in :name ,name))))

#|
Error NOT-AUTHORIZED

Should be signaled when the user tries to perform an action he is not
authorized to perform.

Arguments:
* COMMAND: the command the user was not authorized to perform.
|#
(define-gateway-error not-authorized
    ((command :accessor not-authorized-command
              :initarg :command
              :initform (error "Must provide the command.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((command (not-authorized-command condition)))
    (data-send connection `(:error :type :not-authorize :command ,command))))

#|
Error AUTHENTICATION-FAILURE

Should be signaled when the user attempts to login, but the credentials
provided by him are not valid.
|#
(define-gateway-error authentication-failure ()
    (owner connection condition)
  (declare (ignore owner condition))
  (data-send connection `(:error :type :authentication-failure)))

