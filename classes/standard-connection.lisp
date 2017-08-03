;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-connection.lisp

(in-package :gateway/impl)
(in-readtable protest)

(defclass standard-connection (connection)
  ((%socket :accessor socket-of)
   (%auth :accessor authentication :initform nil)
   (%lock :accessor lock)))

(define-constructor (standard-connection (host "127.0.0.1") (port 65001) socket)
  (unless socket
    (check-type host string)
    (check-type port (unsigned-byte 16))
    (setf socket (change-class (socket-connect host port) 'standard-socket
                               :owner standard-connection)))
  (check-type socket stream-usocket)
  (setf (socket-of standard-connection)
        (change-class socket 'standard-socket :owner standard-connection)
        (lock standard-connection)
        (make-lock (format nil "STANDARD-CONNECTION ~A:~D" host port))))

;;;; KILLABLE
(defmethod deadp ((connection standard-connection))
  (with-lock-held ((lock connection))
    (alivep-internal connection)
    (not (open-stream-p (stream-of connection)))))

(defmethod kill ((connection standard-connection))
  (with-lock-held ((lock connection))
    (connection-kill connection))
  (values))

(defmethod stream-of ((connection connection))
  (socket-stream (socket-of connection)))

(defun alivep-internal (connection)
  (handler-case
      (peek-char-no-hang (stream-of connection))
    (error () (connection-kill connection))))

(defun connection-kill (connection)
  (socket-close (socket-of connection)))

;;;; READYP
(defmethod readyp ((connection standard-connection))
  (or (not (open-stream-p (stream-of connection)))
      (with-connection (connection)
        (connection-readyp connection))))

(defun connection-readyp (connection)
  (peek-char-no-hang (stream-of connection)))

;;;; CONNECTION-RECEIVE/CONNECTION-SEND
(defmacro with-connection ((connection) &body body)
  `(when (alivep ,connection)
     (handler-case
         (with-lock-held ((lock ,connection))
           ,@body)
       (error (e)
         (declare (ignorable e))
         ;; TODO add logging (again)
         ;; (note "[!] Connection: ~A~%" e)
         (kill ,connection)
         (error e)))))

(defmethod connection-receive ((connection standard-connection))
  (if (deadp connection)
      (values nil nil)
      (with-connection (connection)
        (if (connection-readyp connection)
            (multiple-value-bind (message condition)
                (safe-read (stream-of connection))
              (if (and (null message) (eq condition :incomplete-input))
                  (values nil t)
                  (values message t)))
            (values nil t)))))

(defmethod connection-send ((connection standard-connection) object)
  (with-connection (connection)
    (let ((sexp (serialize object :type :string)))
      (fformat (stream-of connection) sexp)
      t)))



(defun make-connection-pair ()
  (let* ((socket-listen (socket-listen "127.0.0.1" 0))
         (port (get-local-port socket-listen))
         (socket-connect (socket-connect "127.0.0.1" port))
         (socket-accept (socket-accept socket-listen)))
    (socket-close socket-listen)
    (values (make-instance 'standard-connection :socket socket-connect)
            (make-instance 'standard-connection :socket socket-accept))))

(define-test-case standard-connection-unit
    (:description "Unit tests for STANDARD-CONNECTION."
     :tags (:connection :unit)
     :type :unit-suite))

(define-test standard-connection-unit
  (finalized-let*
      ((conns (multiple-value-list (make-connection-pair))
              (mapc #'kill conns)))
    #1?(is (eq t (connection-send (first conns) '(1 2 3 4))))
    #2?(is (readyp (second conns)))
    (multiple-value-bind (message alivep) (connection-receive (second conns))
      #3?(is (not (readyp (second conns))))
      #4?(is (equal message '(1 2 3 4)))
      #5?(is (eq alivep t)))
    (fformat (stream-of (first conns)) "(")
    #6?(is (readyp (second conns)))
    (multiple-value-bind (message alivep) (connection-receive (second conns))
      #7?(is (not (readyp (second conns))))
      #8?(is (null message))
      #9?(is (eq alivep t)))
    (kill (first conns))
    #10?(is (wait () (readyp (second conns))))
    (multiple-value-bind (message alivep) (connection-receive (second conns))
      #11?(is (null message))
      #12?(is (null alivep)))))

(define-test-case standard-connection-send-receive
    (:description "Test of sending and receiving data for STANDARD-CONNECTIONs."
     :tags (:connection :protocol)
     :type :protocol)
  :arrange
  1 "Create connection 1 (server)."
  2 "Create connection 2 (client)."
  :act
  3 "Send test data from connection 1."
  :assert
  4 "Assert connection 2 is ready."
  5 "Assert the received data matches the data that was sent."
  :act
  6 "Send test data from connection 2."
  :assert
  7 "Assert connection 3 is ready."
  8 "Assert the received data matches the data that was sent.")

(define-test standard-connection-send-receive
  (finalized-let*
      ((socket-listen (socket-listen "127.0.0.1" 0)
                      (socket-close socket-listen))
       (port (get-local-port socket-listen))
       (connection-1 #1?(make-instance 'standard-connection :port port)
                     (kill connection-1))
       (socket-accept (socket-accept socket-listen))
       (connection-2 #2?(make-instance 'standard-connection
                                       :socket socket-accept)
                     (kill connection-2)))
    (let ((test-cases '((1 2 3 4 5 6 7 8 9 0)
                        (#:a #:b #:c #:d #:e #:f (#:g)
                         ((((#:h #:i #:j #:k (#:l 2000) #:m #:n)))))
                        (#:lorem #:ipsum #:dolor #:sit #:amet)
                        ("a" #:a "a" #:a "a" "b"))))
      (labels ((test-case (x y data)
                 #3?(connection-send x data)
                 #4?(is (wait () (readyp y)))
                 #5?(is (data-equal data (connection-receive y)))
                 #6?(connection-send y data)
                 #7?(is (wait () (readyp x)))
                 #8?(is (data-equal data (connection-receive x)))))
        (mapc (alexandria:curry #'test-case connection-1 connection-2)
              test-cases)))))

(define-test-case standard-connection-death
    (:description "Test of KILLABLE protocol for STANDARD-CONNECTIONs."
     :tags (:protocol :killable :connection)
     :type :protocol)
  :arrange
  1 "Create connections."
  2 "Assert connection 1 is alive."
  3 "Assert connection 2 is alive."
  :act
  4  "Kill connection 1."
  :assert
  5 "Assert connection 1 is dead."
  6 "Assert connection 2 is dead.")

(define-test standard-connection-death
  (finalized-let*
      ((conns (multiple-value-list #1?(make-connection-pair))
              (mapc #'kill conns)))
    #2?(is (alivep (first conns)))
    #3?(is (alivep (second conns)))
    #4?(kill (first conns))
    #5?(is (deadp (first conns)))
    #6?(is (deadp (second conns)))))
