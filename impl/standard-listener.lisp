;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-listener.lisp

(in-package #:gateway)

(defclass standard-listener (listener)
  ((%socket :accessor socket
	    :type socket)
   (%thread :accessor thread)
   (%host :reader host
	  :initarg :host
	  :initform (error "No HOST provided.")
	  :type string)
   (%port :reader port
	  :initarg :port
	  :initform (error "No PORT provided.")
	  :type (integer 0 65535)) 
   (%crown :accessor crown
	   :initarg :crown
	   :initform nil)))

(defconstructor (standard-listener) 
  (let ((host (host standard-listener)) (port (port standard-listener)))
    (setf (socket standard-listener) (socket-listen host port :reuseaddress t))
    (setf (thread standard-listener) (%make-listener-thread standard-listener))))

(defun %listener (listener)
  (check-type listener listener)
  (format t "[~~] Listener: starting.~%")
  (unwind-protect
       (loop (%listener-loop listener)) 
    (kill listener)
    (format t "[!] Listener: thread killed.~%")))

(defun %listener-loop (listener)
  (let ((socket (socket-accept (wait-for-input (socket listener)))))
    (format t "[~~] Listener: got a connection, ~{~A.~A.~A.~A~}:~S.~%"
	    (coerce (get-peer-address socket) 'list)
	    (get-peer-port socket))
    (let ((connection (make-instance 'standard-connection :type :ready :socket socket)))
      (if (crown listener)
	  (with-lock-held ((n-connections-lock (crown listener)))
	    (push connection (n-connections (crown listener))))
	  (progn 
	    (fformat (stream-of connection) "(GATEWAY DOWN)~%")
	    (kill connection))))))

(defun %make-listener-thread (listener) 
  (let ((host (host listener)) (port (port listener)))
    (make-thread (lambda () (%listener listener))
		 :name (format nil "Gateway - crown listener on ~A:~S" host port))))

(defmethod kill ((listener standard-listener)) 
  (socket-close (socket listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (values))
