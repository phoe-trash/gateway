;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; commands.lisp

(in-package #:gateway)

;;;; COMMANDS

(defvar %command-data% (make-hash-table))

(defun %execute (command plist)
  (assert (proper-list-p plist))
  (assert (identity command))
  (let* ((fn (gethash command %command-data%)))
    (if (null fn)
        (error "Command ~S not found." command)
        (funcall fn plist))))

(defmacro defcommand (name keyword-list &body body)
  (let* ((gensym-sexp (gensym "COMMAND"))
         (args (list gensym-sexp))
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',name %command-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))
