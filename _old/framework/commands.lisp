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
  (let ((data-word (string command)))
    (multiple-value-bind (fn foundp) (gethash data-word %command-data%)
      (if foundp
          (funcall fn plist owner connection)
          (execute-operation 'unknown-command :command command
                                              :connection connection)))))

(defmacro defcommand (name (owner-var connection-var)
                      keyword-list &body body)
  (let* ((gensym-sexp (gensym "COMMAND"))
         (args (list gensym-sexp owner-var connection-var))
         (let-list (%defcommand-let-list keyword-list gensym-sexp))
         (handle-error (gensym "HANDLE-ERROR"))
         (handle-condition (gensym "HANDLE-CONDITION")))
    `(setf (gethash ',(string name) %command-data%)
           (lambda ,args
             (declare (ignorable ,gensym-sexp))
             (note "[G] Executing command ~S.~%" ',name)
             (let ((,owner-var ,owner-var)
                   (,connection-var ,connection-var)
                   ,@let-list)
               (flet ((,handle-error (x)
                        (handle-gateway-error ,owner-var ,connection-var x))
                      (,handle-condition (x)
                        (handle-gateway-condition ,owner-var ,connection-var x)))
                 (handler-case
                     (handler-bind ((gateway-condition #',handle-condition))
                       (progn
                         ,@(%defcommand-type-list keyword-list)
                         ,@body))
                   (gateway-error (error)
                     (,handle-error error)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %defcommand-let-list (keyword-list gensym)
    (flet ((fn (x) `(,(first x) (data-getf ,gensym ',(or (second x) (first x)))))
           (listify (x) (list (intern (string x) :gateway) x))
           (strip (x) (if (listp x) (first x) x)))
      (let ((keyword-list (mapcar #'strip keyword-list)))
        (mapcar #'fn (mapcar #'listify keyword-list)))))

  (defun %defcommand-type-list (keyword-list)
    (let ((keyword-list (remove-if-not #'listp keyword-list)))
      (flet ((generate (x)
               (let ((symbol (intern (string (first x)) :gateway)))
                 `(unless (typep ,symbol ',(second x))
                    (error 'illegal-argument :var ',(first x) :type ',(second x)
                                             :value ,symbol)))))
        (mapcar #'generate keyword-list)))))
