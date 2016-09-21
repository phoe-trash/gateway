;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem-commands.lisp

(in-package #:gateway)

(defvar *gem-n-handlers* (make-hash-table :test #'equal))
(defvar *gem-e-handlers* (make-hash-table :test #'equal))
(defvar *gem-i-handlers* (make-hash-table :test #'equal))

;; TODO no need for this
(defun %gem-command-replace-keywords (pattern)
  (loop for element in pattern
        if (keywordp element)
          collect (make-symbol (symbol-name element))
        else collect element))

;; TODO rewrite this using verify-arguments and functions from hashtables
(defmacro defcommand ((pattern types) (crown-var connection-var arguments-var) &body body)
  (let ((pattern (%gem-command-replace-keywords pattern)))
    (multiple-value-bind (params optional rest keys allowp aux) (parse-ordinary-lambda-list pattern)
      nil)))

;; TODO remember that we are storing STRINGS in hashtables, grabbed using SYMBOL-NAME on read symbols

(defcommand ((:ping ping-data) (:n :e :i))
    (crown connection arguments)
  (format t "[~~] Gem: ping-pong: ~S~%" ping-data) 
  (send connection (rplaca command 'pong)))

(defcommand ((:open-gateway &optional open-data) (:n))
    (crown connection arguments)
  (when open-data
    (format t "[~~] Gem: accepting E-connection: ~S.~%" open-data)
    (format t "[~~] Gem: accepting E-connection.~%"))
  (send connection (list 'ok command)) 
  (with-lock-held ((n-lock crown))
    (deletef (n-connections crown) connection))
  (with-lock-held ((e-lock crown))
    (pushnew connection (e-connections crown))))

(defun verify-arguments (function &rest arguments)
  (funcall (generate-matcher (arglist function)) arguments))

(defun generate-matcher (lambda-list)
  (handler-bind ((style-warning #'muffle-warning))
    (compile
     nil
     (let ((candidate-name (gensym)))
       `(lambda (,candidate-name) 
          (match ,candidate-name
            ((lambda-list ,@lambda-list) t)))))))

(verify-arguments #'+ 1 2) ; => T
(verify-arguments #'princ 1 2) ; => T
(verify-arguments #'princ 1 2 3) ; => NIL
