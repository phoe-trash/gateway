;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-connection.lisp

(in-package #:gateway)

(defclass standard-connection (connection)
  ((%socket :accessor socket
	    :type socket)
   (%stream :accessor stream-of
	    :type stream)
   (%lock :initform (make-lock)
	  :accessor lock)
   (%auth :initform nil
	  :initarg :auth
	  :accessor auth)))

(defconstructor (standard-connection type (host "127.0.0.1") (port 65001) socket)
  (check-type type (member :listen :client :accept :ready))
  (check-type host string)
  (check-type port (integer 0 65535))
  (check-type socket (or null usocket))
  (with-lock-held ((lock standard-connection))
    (let* ((accepted-socket (when (eq type :accept) (socket-accept socket)))
	   (socket (case type
		     (:listen (socket-listen "127.0.0.1" port :reuseaddress t)) 
		     (:client (socket-connect host port))
		     (:accept accepted-socket)
		     (:ready socket)))
	   (stream (case type
		     (:listen nil)
		     (:client (socket-stream socket))
		     (:accept (socket-stream accepted-socket))
		     (:ready (socket-stream socket))))) 
      (setf (socket standard-connection) socket
	    (stream-of standard-connection) stream)
      (with-lock-held (*cache-lock*)
	(setf (gethash standard-connection *connection-cache*)
	      standard-connection)))))

(defmethod receive ((connection standard-connection) &key parent)
  (when (alivep connection)
    (handler-case
        (with-lock-held ((lock connection))
          (parse (safe-read (stream-of connection)) parent))
      (end-of-file ()
        (kill connection)))))

(defmethod send ((connection standard-connection) object) 
  (with-lock-held ((lock connection))
    (let ((sexp (sexp object))
	  *print-pretty*) 
      (format (stream-of connection) "~S~%" sexp)
      (force-output (stream-of connection)))))

(defmethod readyp ((connection standard-connection))
  (with-lock-held ((lock connection))
    (handler-case
        (peek-char-no-hang (stream-of connection))
      (end-of-file ()
        t))))

(defmethod alivep ((connection standard-connection))
  (with-lock-held ((lock connection))
    (open-stream-p (stream-of connection))))

(defmethod kill ((connection standard-connection))
  (with-lock-held ((lock connection))
    (when (stream-of connection)
      (close (stream-of connection)))
    (when (socket connection)
      (socket-close (socket connection))))
  (with-lock-held (*cache-lock*)
    (remhash connection *connection-cache*))
  (values))
