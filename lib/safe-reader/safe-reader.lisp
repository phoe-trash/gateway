;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READER
;;;; © Michał "phoe" Herda 2016

(in-package #:gateway)

(defun safe-read-line (stream)
  (with-output-to-string (result)
    (do ((char-counter 0)
	 (char (read-char stream nil #\Nul)
	       (read-char stream nil #\Nul)))
	((or (eql char #\Newline)
	     (eql char #\Nul)))
      (when (< *max-input-size* (incf char-counter))
	(signal (make-condition 'input-size-exceeded)))
      (princ char result))))
