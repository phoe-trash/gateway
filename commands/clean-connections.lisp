;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; clean-connections.lisp

(in-package #:gateway)

#|
Command CLEAN-CONNECTIONS

This command cleans a list of connections from dead ones.

Arguments:
* LOCK: a lock to be held, or NIL if no lock is required.
* GETTER: a function of no arguments to retrieve the connections.
* SETTER: a function of one argument to set the connections.

TODO rename commands to operations, since commands will be textual
SEXPs sent between client and server.
|#
(defcommand clean-connections (:lock :getter :setter)
  (with-lock-held ((or lock (make-lock)))
    (let ((result (delete-if #'deadp (funcall getter))))
      (funcall setter result))))

(deftest test-command-clean-connections
  (finalized-let* ((crown (%make-crown) (kill crown))
                   (lock (n-lock crown))
                   (getter (curry #'n-connections crown))
                   (setter (lambda (x) (setf (n-connections crown) x))))
    (is (wait () (= 1 (length (n-connections crown)))))
    (mapc #'kill (n-connections crown))
    (execute 'clean-connections :lock lock :getter getter :setter setter)
    (is (= 0 (length (n-connections crown))))))
