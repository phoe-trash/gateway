;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    ((var :reader illegal-argument-var
          :initarg :var
          :initform (error "Must provide var."))
     (value :reader illegal-argument-value
            :initarg :value
            :initform (error "Must provide value."))
     (type :reader illegal-argument-type
           :initarg :type
           :initform (error "Must provide type.")))
    (owner connection condition)
    (((var (illegal-argument-var condition))
      (value (illegal-argument-value condition))
      (type (illegal-argument-type condition)))
     ("[!] Illegal argument for ~A: ~S, which is not of type ~S.~%"
      var value type)
      (declare (ignore owner))
      (data-send connection `(:error :type :illegal-argument
                                     :var ,var :value ,value
                                     :expected-type ,type))))

(deftest test-error-illegal-argument
  (with-crown-and-connections crown (connection) ()
    (flet ((response (var value)
             `(:error :type :illegal-argument
                      :var ,var :value ,value :expected-type :string)))
      (%test connection
             '(:login :username 2 :password "foo")
             (response :username 2)
             '(:login :username "foo" :password :password)
             (response :password :password)
             '(:login :username "foo" :password (1 2 3 4))
             (response :password '(1 2 3 4))))))
