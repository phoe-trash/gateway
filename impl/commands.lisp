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
         (let-list (%data-getf-let-list keyword-list gensym-sexp)))
    `(setf (gethash ',(string name) %command-data%)
           (lambda ,args
             (declare (ignorable ,@args))
             (let ,let-list
               ,@body)))))
