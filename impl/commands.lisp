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

(defcommand login (owner connection)
            (:username :password)
  (check-type owner crown))

(defcommand ping (owner connection)
            (:data)
  (note "[C] Got pinged with data ~S.~%" data)
  (data-send connection (list :pong :data data)))

(deftest test-command-ping
  (multiple-value-bind (crown n-host n-port i-host i-port)
      (%make-crown-with-listed-ports)
    (declare (ignore i-host i-port))
    (unwind-protect
         (finalized-let* ((connection (%make-connection n-host n-port)
                                      (kill connection)))
           (data-send connection '(:ping :data (1 2 3 4 5)))
           (wait () (readyp connection))
           (is (wait () (data-equal (data-receive connection)
                                    '(:pong :data (1 2 3 4 5))))))
      (kill crown))))
