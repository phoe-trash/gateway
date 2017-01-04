;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; commands.lisp

(in-package #:gateway)

;;;; COMMANDS

(defvar %command-data% (make-hash-table))

(defun %execute (command)
  (assert (proper-list-p command))
  (assert (identity command))
  (let* ((data-word (string (first command)))
         (fn (gethash data-word %command-data%)))
    (funcall fn (cdr sexp))))

(defmacro defcommand (name keyword-list &body body)
  (let* ((gensym-sexp (gensym "COMMAND"))
         (args (list gensym-sexp))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',name %command-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))

#|
Command CLEAN-CONNECTIONS

This command cleans a list of connections from dead ones.

Arguments:
* LOCK: a lock to be held, or NIL if no lock is required.
* GETTER: a function of no arguments to retrieve the connections.
* SETTER: a function of one argument to set the connections.
|#
(defcommand clean-connections (lock getter setter)
  (with-lock-held ((or lock (make-lock)))
    (let ((result (delete-if #'deadp (funcall getter))))
      (funcall setter result))))
