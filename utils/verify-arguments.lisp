;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/verify-arguments.lisp

(in-package #:gateway/utils)

(defun verify-arguments (function &rest arguments)
  "Returns true if the provided arguments constitute a valid lambda list for the
provided function."
  (labels ((compile-lambda (lambda-expr)
             (compile nil lambda-expr))
           (clean-pred (x)
             (and (symbolp x) (not (member x lambda-list-keywords))))
           (clean-symbols (lambda-list)
             (substitute-if '_ #'clean-pred lambda-list))
           (generate-matcher (lambda-list)
             (compile-lambda
              (let ((candidate-name (gensym)))
                `(lambda (,candidate-name)
                   (match ,candidate-name
                     ((lambda-list ,@(clean-symbols lambda-list)) t)))))))
    (funcall (generate-matcher (arglist function)) arguments)))
