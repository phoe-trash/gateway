;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/with-restartability.lisp

(in-package :gateway/framework)

(defun %w-r-report (killable-name)
  `(lambda (stream)
     (format stream
             "Abort the current iteration and send the ~A back to its loop."
             (string-downcase (string (or ',killable-name "thread"))))))

(defmacro with-restartability ((&optional killable) &body body)
  "This macro serves two purposes. First, it provides a semiautomatic means
of automatically restarting the function body by means of providing a RETRY
restart; second, in case the retry is not chosen, it provides an optional
facility of automatically killing a killable object by means of UNWIND-PROTECT.

This macro is meant for being used inside functions passed to BT:MAKE-THREAD."
  `(unwind-protect
        (tagbody :start
           (restart-case (progn ,@body)
             (retry () :report ,(%w-r-report killable) (go :start))))
     ,(when killable `(kill ,killable))))
