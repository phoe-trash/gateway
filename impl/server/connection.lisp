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

(defconstructor (connection type (host "") (port 65001) socket)
  "There are three connection types:
:LISTEN - this creates a listening connection;
:ACCEPT - this creates an accepted connection;
:CLIENT - this creates an outgoing connection."
  (cond ((not (stringp host))
	 (error "HOST must be a string."))
	((or (not (integerp port))
	     (not (<= 0 port 65535)))
	 (error "PORT must be an integer between 0 and 65535."))
	(t
	 (with-lock-held ((lock connection))
	   (let* ((socket (ecase type
			    (:listen (socket-listen "127.0.0.1" port :reuse-address t))
			    (:accept (or socket (error "No SOCKET provided.")))
			    (:client (socket-connect host port))))
		  (stream (ecase type
			    (:listen nil)
			    (:accept (socket-stream socket))
			    (:client (socket-stream socket)))))
	     (with-lock-held (*cache-lock*)
	       (push socket *socket-cache*))
	     (setf (socket connection) socket)
	     (when stream
	       (setf (stream-of connection) stream)))))))

(defmethod input (connection &key safe-p)
  (with-lock-held ((lock connection))
    (if safe-p
	(safe-read connection)
	(unsafe-read connection))))

(defmethod kill (connection)
  (with-lock-held ((lock connection))
    (close (stream-of connection))
    (socket-close (socket connection))))

(defmethod output (object connection)
  (format (stream-of connection) "~S~%" (get-sexp object)))
