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
|#
(defcommand clean-connections (lock getter setter)
  (with-lock-held ((or lock (make-lock)))
    (let ((result (delete-if #'deadp (funcall getter))))
      (funcall setter result))))

;; (defcommand test-command (arg-1 arg-2)
;;   (let ((format-string (cat "This is a test command! "
;;                             "ARG-1 is ~S and ARG-2 is ~S.")))
;;     (format t format-string arg-1 arg-2)))
