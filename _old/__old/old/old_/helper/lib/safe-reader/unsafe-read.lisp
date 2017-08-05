;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READER
;;;; © Michał "phoe" Herda 2016

(in-package #:gateway)

(defun unsafe-read (connection)
  (let ((buffer (buffer-of connection)))
    (handler-case
        (if (string= "" buffer)
            (%unsafe-read-no-buffer connection)
            (%unsafe-read-buffer connection))
      (incomplete-input () (values nil :incomplete-input))
      (error (error)
        (kill connection)
        (values nil (condition-key error))))))

(macrolet
    ((unsafe-read-handler-case (&body body)
       (let ((gensym (gensym)))
	 `(handler-case
	      (progn
		(let ((,gensym (progn ,@body)))
		  (setf (buffer-of connection) "")
		  (values ,gensym nil)))
	    (end-of-file ()
	      (setf (buffer-of connection) (cat line (string #\Newline)))
	      (signal (make-condition 'incomplete-input)))))))

  (defun %unsafe-read-no-buffer (connection)
    (let ((line (read-limited-line (stream-of connection))))
      (unsafe-read-handler-case
       (read-from-string line))))

  (defun %unsafe-read-buffer (connection)
    (let* ((line (read-limited-line (stream-of connection)))
	   (buffer (buffer-of connection))
	   (line (cat line buffer)))
      (unsafe-read-handler-case
       (read-from-string (cat buffer line))))))

