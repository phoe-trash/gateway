;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-acceptor.lisp

(in-package :gateway/impl)

(defclass standard-acceptor (acceptor)
  ((%socket :accessor socket)
   (%thread :accessor thread)
   (%name :accessor name)
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function."))))

(define-constructor (standard-acceptor (host "127.0.0.1") (port 0))
  (check-type host string)
  (check-type port (unsigned-byte 16))
  (let* ((socket (socket-listen "127.0.0.1" port :reuseaddress t))
         (name (%acceptor-constructor-name socket))
         (fn (curry #'%acceptor-loop standard-acceptor)))
    (setf (socket standard-acceptor) socket
          (name standard-acceptor) name
          (thread standard-acceptor) (make-thread fn :name name))))

(defun %acceptor-constructor-name (socket)
  (format nil "Gateway - Acceptor for ~{~D.~D.~D.~D~}:~D"
          (coerce (get-local-name socket) 'list)
          (get-local-port socket)))

(defun %acceptor-loop (acceptor)
  (with-restartability (acceptor)
    (loop
      (let* ((socket (socket acceptor))
             (accept (socket-accept (wait-for-input socket)))
             (connection (make-instance 'standard-connection :socket accept)))
        ;; (note "[.] ~A: got a connection, ~{~A.~A.~A.~A~}:~S.~%"
        ;;       (name acceptor)
        ;;       (coerce (get-peer-address accept) 'list)
        ;;       (get-peer-port accept))
        (funcall (handler acceptor) connection)))))

(defmethod deadp ((acceptor standard-acceptor))
  (not (thread-alive-p (thread acceptor))))

(defmethod kill ((acceptor standard-acceptor))
  (unless (eq (thread acceptor) (current-thread))
    (destroy-thread (thread acceptor)))
  (unless (deadp acceptor)
    (socket-close (socket acceptor)))
  (values))



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
    #2?(is (alivep acceptor))
    #3?(kill acceptor)
    #4?(is (wait () (deadp acceptor)))))

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
         (host (get-local-address (socket acceptor)))
         (port (get-local-port (socket acceptor)))
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
