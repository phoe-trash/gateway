;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; commands.lisp

(in-package #:gateway)

;;;; COMMANDS

(defvar %command-data% (make-hash-table :test #'equalp))

(defun %execute-command (owner command connection)
  (check-type owner crown)
  (assert (proper-list-p command))
  (assert (identity command))
  (let* ((data-word (string (first command)))
         (fn (gethash data-word %command-data%)))
    (if (null fn)
        (error "Command ~S not found." command)
        (progn (note "[G] Executing command ~A.~%" command)
               (funcall fn owner command connection)))))

(defmacro defcommand (name (owner-var command-var connection-var) keyword-list &body body)
  (let* ((gensym-sexp (gensym "COMMAND"))
         (args (list gensym-sexp owner-var command-var connection-var))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',name %command-data%)
           (lambda ,args
             (let ,let-list
               ,@body)))))
