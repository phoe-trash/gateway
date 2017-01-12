;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; illegal-argument.lisp

(in-package #:gateway)

#|
Error ILLEGAL-ARGUMENT

Should be signaled when a command contains an argument of an
unexpected type.

Arguments:
* VAR: the variable which contained the value.
* VALUE: the value in question.
* TYPE: the expected type of the value.
|#

(define-gateway-error illegal-argument
    ((var :accessor illegal-argument-var
          :initarg :var
          :initform (error "Must provide var."))
     (value :accessor illegal-argument-value
            :initarg :value
            :initform (error "Must provide value."))
     (type :accessor illegal-argument-type
           :initarg :type
           :initform (error "Must provide type.")))
    (owner connection condition)
  (declare (ignore owner))
  (let ((var (illegal-argument-var condition))
        (value (illegal-argument-value condition))
        (type (illegal-argument-type condition)))
    (note "[!] Illegal argument for ~A: ~S, which is not ~S.~%" var value type)
    (data-send connection `(:error :type :illegal-argument
                                   :var ,var :value ,value :type ,type))))

(deftest test-error-illegal-argument
  (with-crown-and-connections crown (connection) ()
    (flet ((response (var value)
             `(:error :type :illegal-argument
                      :var ,var :value ,value :type :string)))
      (data-send connection '(:login :username 2 :password "foo"))
      (is (wait () (data-equal (data-receive connection)
                               (response :username 2))))
      (data-send connection '(:login :username "foo" :password :password))
      (is (wait () (data-equal (data-receive connection)
                               (response :password :password))))
      (data-send connection '(:login :username "foo" :password (1 2 3 4)))
      (is (wait () (data-equal (data-receive connection)
                               (response :password '(1 2 3 4))))))))
