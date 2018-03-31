;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-acceptor.lisp

(in-package :gateway/impl)

(in-readtable protest)

(defclass standard-acceptor (acceptor)
  ((%socket :accessor socket-of)
   (%thread :accessor thread)
   (%name :accessor name)
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function.")))
  (:documentation #.(format nil "A standard acceptor implementation, with a ~
single server socket. Whenever a socket connection is initiated from outside, ~
a connection is created and the handler function is called on it.")))

(define-print (standard-acceptor stream)
  (if (alivep standard-acceptor)
      (let ((socket (socket-of standard-acceptor)))
        (format stream "(~{~D.~D.~D.~D~}:~D, ALIVE)"
                (coerce (get-local-name socket) 'list)
                (get-local-port socket)))
      (format stream "(DEAD)")))

(define-constructor (standard-acceptor (host "127.0.0.1") (port 0))
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (v:trace :gateway "Standard acceptor starting at ~A:~D." host port)
  (let* ((socket (socket-listen "127.0.0.1" port :reuseaddress t))
         (name (acceptor-constructor-name socket))
         (fn (curry #'acceptor-loop standard-acceptor)))
    (setf (socket-of standard-acceptor) socket
          (name standard-acceptor) name
          (thread standard-acceptor) (make-thread fn :name name))))

(defun acceptor-constructor-name (socket)
  (format nil "Gateway - Acceptor for ~{~D.~D.~D.~D~}:~D"
          (coerce (get-local-name socket) 'list)
          (get-local-port socket)))

(defun acceptor-loop (acceptor)
  (with-restartability (acceptor)
    (loop
      (let* ((socket (socket-of acceptor))
             (accept (socket-accept (wait-for-input socket)))
             (connection (make-instance 'standard-connection :socket accept)))
        (funcall (handler acceptor) connection)))))

(defmethod deadp ((acceptor standard-acceptor))
  (not (thread-alive-p (thread acceptor))))

(defmethod kill ((acceptor standard-acceptor))
  (unless (eq (thread acceptor) (current-thread))
    (destroy-thread (thread acceptor)))
  (unless (deadp acceptor)
    (socket-close (socket-of acceptor)))
  (values))

;;; TESTS

(define-test-case standard-acceptor-death
    (:description "Test of KILLABLE protocol for STANDARD-ACCEPTOR."
     :tags (:protocol :killable :connection)
     :type :protocol)
  :arrange
  1 "Create an acceptor."
  2 "Assert acceptor is alive."
  :act
  3 "Kill acceptor."
  :assert
  4 "Assert acceptor is dead.")

(define-test standard-acceptor-death
  (let ((acceptor #1?(make-instance 'standard-acceptor
                                    :handler (constantly nil))))
    (unwind-protect #2?(is (alivep acceptor))
      #3?(kill acceptor)
      #4?(is (wait () (deadp acceptor))))))

(define-test-case standard-acceptor-unit
    (:description "Unit tests for STANDARD-ACCEPTOR."
     :tags (:acceptor :unit)
     :type :unit-suite))

(define-test standard-acceptor-unit
  (let* ((connections nil)
         (handler (lambda (x) (push x connections))))
    (finalized-let*
        ((acceptor #1?(make-instance 'standard-acceptor :host "127.0.0.1"
                                                        :port 0
                                                        :handler handler)
                   (kill acceptor))
         (host (get-local-address (socket-of acceptor)))
         (port (get-local-port (socket-of acceptor)))
         (socket-1 #2?(socket-connect host port)
                   (socket-close socket-1)
                   (is (not (open-stream-p (socket-stream socket-1)))))
         (socket-2 #3?(socket-connect host port)
                   (socket-close socket-2)
                   (is (not (open-stream-p (socket-stream socket-2)))))
         (socket-3 #4?(socket-connect host port)
                   (socket-close socket-3)
                   (is (not (open-stream-p (socket-stream socket-3))))))
      #5?(is (wait (20) (= 3 (length connections)))))))
