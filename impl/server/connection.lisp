;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass connection ()
  ((%socket :accessor socket
	    :type socket)
   (%stream :accessor stream-of
	    :type stream)
   (%buffer :initform ""
	    :accessor buffer-of
	    :type string)
   (%lock :initform (make-lock)
	  :accessor lock)
   (%auth :initform nil
	  :initarg :auth
	  :accessor auth)))

(defconstructor (connection type host port socket)
  "There are three connection types:
:LISTEN - this creates a listening connection;
:ACCEPT - this creates an accepted connection;
:CLIENT - this creates an outgoing connection."
  (with-lock-held ((lock connection))
    (let* ((socket (ecase type
		     (:listen (%connection-server port))
		     (:accept (or socket (error "No SOCKET provided.")))
		     (:client (%connection-client host port))))
	   (stream (ecase type
		     (:listen nil)
		     (:accept (socket-stream socket))
		     (:client (socket-stream socket)))))
      (with-lock-held (*cache-lock*)
	(push socket *socket-cache*))
      (setf (socket connection) socket)
      (when stream
	(setf (stream-of connection) stream)))))

(defun %connection-server (port)
  (cond ((or (not (integerp port))
	     (not (<= 0 port 65535)))
	 (error "PORT must be an integer between 0 and 65535."))
	(t
	 (socket-listen "127.0.0.1" port :reuse-address t))))

(defun %connection-client (host port)
  (cond ((not (stringp host))
	 (error "HOST must be a string."))
	((or (not (integerp port))
	     (not (<= 0 port 65535)))
	 (error "PORT must be an integer between 0 and 65535."))
	(t
	 (socket-connect host port))))

(define-condition incomplete-input () ())
(define-condition malformed-input (error) ())

(defun input ((connection connection) &key safe-p)
  (with-lock-held (lock connection)
    (if safe-p
	(safe-read connection)
	(unsafe-read connection))))

(defmethod kill (connection)
  (with-lock-held ((lock connection))
    (close (stream-of connection))
    (socket-close (socket connection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UNSAFE-READ

(defun unsafe-read (connection)
  (let ((buffer (buffer-of connection)))
    (handler-case
	(if (string= "" buffer)
	    (%unsafe-read-no-buffer connection)
	    (%unsafe-read-buffer connection))
      (incomplete-input () (values nil :incomplete-input)))))

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
    (let ((line (read-line (stream-of connection))))
      (unsafe-read-handler-case
       (read-from-string line))))

  (defun %unsafe-read-buffer (connection)
    (let* ((line (read-line (stream-of connection)))
	   (buffer (buffer-of connection))
	   (line (cat line buffer)))
      (unsafe-read-handler-case
       (read-from-string (cat buffer line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTILITIES FOR SAFE-READ

(defparameter %safe-readtable% (copy-readtable))

(let ((*readtable* %safe-readtable%))
  (flet ((signal-malformed-input (stream ignore)
	   (declare (ignore stream ignore))
	   (signal 'malformed-input)))
    (set-macro-character #\: #'signal-malformed-input)
    (dotimes (i 256)
      (let* ((char (code-char i))
	     (macro-char (get-macro-character char)))
	(unless (or (null char)
		    (eql char #\()
		    (eql char #\))
		    (eql char #\")
		    (null macro-char))
	  (set-macro-character char #'signal-malformed-input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAFE-READ

(defun safe-read (connection)
  (let ((buffer (buffer-of connection)))
    (handler-case
	(if (string= "" buffer)
	    (%safe-read-no-buffer connection)
	    (%safe-read-buffer connection))
      (incomplete-input () (values nil :incomplete-input))
      (malformed-input ()
	(kill connection)
	(values nil :malformed-input)))))

(macrolet
    ((with-temp-package (&body body) 
       (let* ((package-name (gensym (cat "TEMP-PKG-" (format nil "~S" (now)) "-")))
	      (gensym (gensym)))
	 `(let ((,gensym (make-package ',package-name)))
	    (unwind-protect (let ((*package* ,gensym))
			      ,@body)
	      (delete-package ,gensym)))))
     (safe-read-handler-case (&body body)
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
    (let ((line (read-line (stream-of connection))))
      (safe-read-handler-case
       (with-temp-package
	 (read-from-string line)))))

  (defun %safe-read-buffer (connection)
    (let* ((line (read-line (stream-of connection)))
	   (buffer (buffer-of connection))
	   (line (cat line buffer)))
      (safe-read-handler-case
       (with-temp-package
	 (read-from-string (cat buffer line)))))))
