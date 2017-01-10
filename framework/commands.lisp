;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; commands.lisp

(in-package #:gateway)

;;;; COMMANDS

(defvar %command-data% (make-hash-table :test #'equalp))

(defun %execute-command (owner connection command plist)
  (check-type owner crown)
  (assert (proper-list-p plist))
  (assert (identity command))
  (let* ((data-word (string command))
         (fn (gethash data-word %command-data%)))
    (if (null fn)
        (error "Command ~S not found." command)
        (progn (note "[G] Executing command ~S.~%" command)
               (funcall fn plist owner connection)))))

(defmacro defcommand (name (owner-var connection-var)
                      keyword-list &body body)
  (let* ((gensym-sexp (gensym "COMMAND"))
         (args (list gensym-sexp owner-var connection-var))
         (let-list (%data-getf-let-list keyword-list gensym-sexp))
         (handle-error (gensym "HANDLE-ERROR"))
         (handle-condition (gensym "HANDLE-CONDITION")))
    `(setf (gethash ',(string name) %command-data%)
           (lambda ,args
             (declare (ignorable ,gensym-sexp))
             ;; (declare (ignorable ,@args))
             (let ((,owner-var ,owner-var)
                   (,connection-var ,connection-var)
                   ,@let-list)
               (flet ((,handle-error (x)
                        (handle-gateway-error ,owner-var ,connection-var x))
                      (,handle-condition (x)
                        (handle-gateway-condition ,owner-var ,connection-var x)))
                 (handler-case
                     (handler-bind ((gateway-condition #',handle-condition))
                       (progn ,@body))
                   (gateway-error (error)
                     (,handle-error error)))))))))
