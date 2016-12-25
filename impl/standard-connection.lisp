;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-connection.lisp

(in-package #:gateway)

(defclass standard-connection (connection)
  ((%socket :accessor socket)
   (%lock :accessor lock
          :initform (make-lock))))

(defconstructor (standard-connection (host "127.0.0.1") (port 65001) socket)
  (unless socket
    (check-type host string)
    (check-type port (unsigned-byte 16))
    (setf socket (change-class (socket-connect host port) 'standard-socket
                               :owner standard-connection)))
  (check-type socket usocket)
  (with-lock-held ((lock standard-connection))
    (setf (socket standard-connection)
          (change-class socket 'standard-socket :owner standard-connection))))

(defmethod stream-of ((connection standard-connection))
  (socket-stream (socket connection)))

(defprint standard-connection
  (print-unreadable-object (obj stream :type t)
    (format stream "~{~D:~D:~D:~D~}:~D"
            (coerce (get-peer-address (socket obj)) 'list)
            (get-peer-port (socket obj)))))

(defmethod data-receive ((connection standard-connection))
  (when (alivep connection)
    (handler-case
        (with-lock-held ((lock connection))
          (safe-read (stream-of connection)))
      (end-of-file ()
        (kill connection)))))

(defmethod data-send ((connection standard-connection) object)
  (when (alivep connection)
    (handler-case
        (with-lock-held ((lock connection))
          (let ((sexp (sexp object))
                (*print-pretty* nil))
            (format (stream-of connection) "~S~%" (unintern-all-symbols sexp))
            (force-output (stream-of connection))))
      (error () (kill connection) t))))

(defmethod readyp ((connection standard-connection))
  (when (alivep connection)
    (handler-case
        (with-lock-held ((lock connection))
          (peek-char-no-hang (stream-of connection)))
      (error () (kill connection) t))))

(defmethod alivep ((connection standard-connection))
  (with-lock-held ((lock connection))
    (open-stream-p (stream-of connection))))

(defmethod kill ((connection standard-connection))
  (with-lock-held ((lock connection))
    (%kill-connection connection))
  (values))

(defun %kill-connection (connection)
  (when (stream-of connection)
    (close (stream-of connection)))
  (when (socket connection)
    (socket-close (socket connection))))


