;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-listener.lisp

(in-package #:gateway)

(defclass standard-listener (listener)
  ((%connection :accessor connection)
   (%thread :accessor thread)
   (%name :accessor name)
   (%getter :accessor getter
            :initarg :getter
            :initform (error "Must define a getter function."))
   (%pusher :accessor pusher
            :initarg :pusher
            :initform (error "Must define a pusher function."))))


(defconstructor (standard-listener)
  (multiple-value-bind (connection-1 connection-2) (make-connection-pair)
    (let ((name "Gateway - Listener")
          (fn (lambda () (%listener-loop-1 standard-listener))))
      (funcall (pusher standard-listener) connection-1)
      (setf (name standard-listener) "Gateway - Listener"
            (connection standard-listener) connection-2
            (thread standard-listener) (make-thread fn :name name)))))

(defun %listener-loop-1 (listener)
  (format t "[~~] ~A: starting.~%" (name listener))
  (unwind-protect
       (%listener-loop-2 listener)
    (kill listener)
    (format t "[!] ~A: killed.~%" (name listener))))

(defun %listener-loop-2 (listener)
  (restart-case
      (loop (%listener-loop-3 listener))
    (retry ()
      :report "Abort the current iteration and send the listener back to its loop."
      (format t "[!] ~A: restarted.~%" (name listener))
      (%listener-loop-2 listener))))

(defun %listener-loop-3 (listener)
  (let* ((sockets (mapcar #'socket (funcall (getter listener))))
         (socket (wait-until (wait-for-input sockets :timeout 0)))
         (connection (owner socket))
         (data (data-receive connection)))
    (when data
      (funcall (pusher listener) (list connection data)))))

(defmacro wait-until (form)
  (with-gensyms (result)
    `(loop for ,result = ,form
           if ,result return ,result)))

(defmethod notify ((listener standard-listener))
  (data-send (connection listener) nil))

(defmethod alivep ((listener standard-listener))
  (thread-alive-p (thread listener)))

(defmethod kill ((listener standard-listener))
  (kill (connection listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (values))

(deftest test-standard-listener
  (let* ((getter (lambda ()))
         (pusher (lambda (x) (kill x)))
         (listener (make-instance 'standard-listener
                                  :getter getter :pusher pusher)))
    (is (alivep listener))
    (kill listener)
    (is (wait () (not (alivep listener)))))
  (let* ((connections nil) (data nil) (lock (make-lock))
         (getter (lambda () (with-lock-held (lock) connections)))
         (pusher (lambda (x) (push x data))))
    (finalized-let* ((listener (make-instance 'standard-listener
                                              :getter getter :pusher pusher)
                               (kill listener)
                               (is (wait () (not (alivep listener)))))
                     (connections-1 (multiple-value-list (make-connection-pair))
                                    (mapc #'kill connections)
                                    (is (wait () (every (compose #'not #'alivep)
                                                        connections-1))))
                     (connections-2 (multiple-value-list (make-connection-pair))
                                    (mapc #'kill connections)
                                    (is (wait () (every (compose #'not #'alivep)
                                                        connections-2)))))
      (push (first connections-1) connections)
      (push (first connections-2) connections)
      (notify listener) 
      )))
