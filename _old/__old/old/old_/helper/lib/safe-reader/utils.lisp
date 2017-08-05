;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READER
;;;; © Michał "phoe" Herda 2016

(in-package #:gateway)

(define-condition incomplete-input () ())
(define-condition malformed-input (error) ())
(define-condition input-size-exceeded (error) ())
(defparameter *max-input-size* (* 128 1024))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter %safe-readtable% (copy-readtable))
  (let ((*readtable* %safe-readtable%))
    (flet ((signal-malformed-input (stream ignore)
	     (declare (ignore stream ignore))
	     (error 'malformed-input))) 
      (dotimes (i 256)
	(let* ((char (code-char i))
	       (macro-char (get-macro-character char)))
	  (unless (or (null char)
		      (eql char #\()
		      (eql char #\))
		      (eql char #\")
		      (null macro-char))
	    (set-macro-character char #'signal-malformed-input))))
      (set-macro-character #\: #'signal-malformed-input))))

(defmacro with-temp-package (&body body) 
  (let* ((package-name (gensym (cat "TEMP-PKG-" (format nil "~S" (now)) "-")))
	 (gensym (gensym)))
    `(let ((,gensym (make-package ',package-name)))
       (unwind-protect (let ((*package* ,gensym))
			 ,@body)
	 (delete-package ,gensym)))))

(defun read-limited-line (stream)
  (let* (result-status
	 (line-read
	   (with-output-to-string (result)
	     (do ((char-counter 0)
		  (char (read-char stream nil #\Nul)
			(read-char stream nil #\Nul)))
		 ((member char '(#\Newline #\Nul))
		  (setf result-status (if (eql char #\Newline) :newline :eof)))
	       (when (< *max-input-size* (incf char-counter))
		 (signal (make-condition 'input-size-exceeded)))
	       (princ char result)))))
    (values line-read result-status)))
