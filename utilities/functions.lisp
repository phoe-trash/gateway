;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; utils.lisp

(in-package #:gateway)

;;;; DATA=-GETF
(defun string=-getf (plist indicator)
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (and (string= key indicator))
          return value))

(defun data-getf (plist indicator)
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (and (symbolp key) (string= key indicator))
          return value))

(defun %data-getf-let-list (keyword-list gensym)
  (flet ((fn (x) `(,(first x) (data-getf ,gensym ',(or (second x) (first x)))))
         (listify (x) (if (listp x) x (list (intern (string x) :gateway) x))))
    (mapcar #'fn (mapcar #'listify keyword-list))))

;;;; DATA-EQUAL
(defun data-equal (object-1 object-2)
  (cond ((and (consp object-1) (consp object-2))
         (every #'data-equal object-1 object-2))
        ((and (symbolp object-1) (symbolp object-2))
         (string= object-1 object-2))
        (t
         (equal object-1 object-2))))

;;;; VARIA
(defun fformat (stream format-string &rest format-args)
  (apply #'format stream format-string format-args)
  (force-output stream))

(defun make-synchro-queue ()
  (make-instance 'synchronized-queue :queue (make-instance 'unbounded-fifo-queue)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cat (&rest strings)
    (apply #'concatenate 'string strings)))

(defun peek-char-no-hang (&optional (input-stream *standard-input*)
                            (eof-error-p t) eof-value recursive-p)
  (let ((character (read-char-no-hang input-stream eof-error-p
                                      eof-value recursive-p)))
    (when character
      (unread-char character input-stream)
      character)))

;;;; UNINTERN-ALL-SYMBOLS
(defun unintern-all-symbols (sexp)
  (cond ((consp sexp)
         (mapcar #'unintern-all-symbols sexp))
        ((symbolp sexp)
         (make-symbol (symbol-name sexp)))
        (t
         sexp)))

;;;; ARGLIST VERIFICATION
(defun compile-lambda (lambda-expr)
  (compile nil lambda-expr))

(defun generate-matcher (lambda-list)
  (labels ((clean-pred (x) (and (symbolp x) (not (member x lambda-list-keywords))))
           (clean-symbols (lambda-list) (substitute-if '_ #'clean-pred lambda-list)))
    (compile-lambda
     (let ((candidate-name (gensym)))
       `(lambda (,candidate-name)
          (match ,candidate-name
            ((lambda-list ,@(clean-symbols lambda-list)) t)))))))

(defun verify-arguments (function &rest arguments)
  (funcall (generate-matcher (arglist function)) arguments))

;;;; REPLACE-ALL
(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

;;;; TESTING
(defun run ()
  (note "~%[T] Beginning tests.~%")
  (1am:run)
  (note "[T] Finished tests.~%~%"))

(defun %test (connection &rest clauses)
  (flet ((%%test (query response)
           (data-send connection query)
           (is (wait () (data-equal (data-receive connection) response)))))
    (loop for (query response) on clauses by #'cddr
          do (%%test query response))))
