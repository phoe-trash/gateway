;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READER
;;;; © Michał "phoe" Herda 2016

(in-package #:gateway)


(defun safe-read (connection)
  (let ((buffer (buffer-of connection)))
    (handler-case
	(if (string= "" buffer)
	    (%safe-read-no-buffer connection)
	    (%safe-read-buffer connection))
      (incomplete-input () (values nil :incomplete-input))
      (error (error)
	(kill connection)
	(values nil (condition-key error))))))

(macrolet
    ((safe-read-handler-case (&body body)
       (let ((gensym (gensym)))
	 `(handler-case
	      (progn 
		(let ((*readtable* %safe-readtable%))
		  (let ((,gensym (progn ,@body)))
		    (setf (buffer-of connection) "")
		    (values ,gensym nil))))
	    (end-of-file ()
	      (setf (buffer-of connection) (cat line (string #\Newline)))
	      (signal (make-condition 'incomplete-input)))
	    (malformed-input (error)
	      (signal error))))))

  (defun %safe-read-no-buffer (connection)
    (let ((line (safe-read-line (stream-of connection))))
      (safe-read-handler-case
       (with-temp-package
	 (read-from-string line)))))

  (defun %safe-read-buffer (connection)
    (let* ((line (safe-read-line (stream-of connection)))
	   (buffer (buffer-of connection))
	   (line (cat line buffer)))
      (safe-read-handler-case
       (with-temp-package
	 (read-from-string (cat buffer line)))))))
