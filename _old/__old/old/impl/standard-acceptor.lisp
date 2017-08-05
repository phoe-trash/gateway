;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-acceptor.lisp

(in-package #:gateway)

(defclass standard-acceptor (acceptor)
  ((%socket :accessor socket)
   (%thread :accessor thread)
   (%owner :accessor owner)
   (%type :initarg :type
          :accessor %type)))

(defconstructor (standard-acceptor (host "127.0.0.1") (port 65001) owner)
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (check-type owner crown)
  (check-type (%type standard-acceptor) (member :n :i))
  (let* ((socket (socket-listen host port :reuseaddress t))
         (type (%type standard-acceptor)))
    (handler-case
        (setf (owner standard-acceptor) owner
              (socket standard-acceptor) socket
              (thread standard-acceptor)
              (make-thread (lambda () (%acceptor standard-acceptor))
                           :name (format nil "Gateway - ~A-acceptor for ~S"
                                         (symbol-name type) owner)))
      (error (e)
        (socket-close socket)
        (error e)))))

(defun %acceptor (acceptor)
  (format t "[~~] ~A-acceptor: starting.~%" (symbol-name (%type acceptor)))
  (unwind-protect
       (%acceptor-thread acceptor)
    (kill acceptor)
    (format t "[!] ~A-acceptor: thread killed.~%" (symbol-name (%type acceptor)))))

(defun %acceptor-thread (acceptor)
  (restart-case
      (loop (%acceptor-loop acceptor))
    (retry ()
      :report "Abort the current iteration and send the acceptor back to its loop."
      (format t "[!] ~A-acceptor: restart invoked.~%" (symbol-name (%type acceptor)))
      (%acceptor-thread acceptor))))

(defun %acceptor-loop (acceptor)
  (let* ((socket (socket acceptor))
         (owner (owner acceptor))
         (type (%type acceptor))
         (accept (socket-accept (wait-for-input socket)))
         (connection (make-instance 'standard-connection :type :ready :socket accept)))
    (format t "[~~] ~A-acceptor: got a connection, ~{~A.~A.~A.~A~}:~S.~%"
            (symbol-name (%type acceptor))
            (coerce (get-peer-address accept) 'list) (get-peer-port accept))
    (etypecase (owner acceptor)
      (crown
       (ecase type
         (:n
          (with-lock-held ((n-lock owner))
            (push connection (n-connections owner))))
         (:i
          (with-lock-held ((i-lock owner))
            (push connection (i-connections owner)))))))))

(defmethod alivep ((acceptor standard-acceptor))
  (thread-alive-p (thread acceptor)))

(defmethod kill ((acceptor standard-acceptor))
  (socket-close (socket acceptor))
  (unless (eq (current-thread) (thread acceptor))
    (destroy-thread (thread acceptor)))
  (values))
