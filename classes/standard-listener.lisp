;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; classes/standard-listener.lisp

(in-package #:gateway/impl)

(defclass standard-listener (listener)
  ((%lock :accessor lock
          :initform (make-lock "Gateway - Listener lock"))
   (%connections :reader connections)
   (%notifier-connection :accessor notifier-connection)
   (%thread :accessor thread)
   (%name :accessor name
          :initform "Gateway - Listener")
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function."))))

(defmethod (setf connections) (new-value (listener standard-listener))
  (prog1 (setf (slot-value listener 'connections) new-value)
    (%notify listener)))

(defun %notify (listener)
  (connection-send (notifier-connection listener) ()))

(define-constructor (standard-listener)
  (multiple-value-bind (connection-1 connection-2) (%make-connection-pair)
    (let ((fn (curry #'%listener-loop standard-listener)))
      (setf (notifier-connection standard-listener) connection-1
            (connections standard-listener) (list connection-2)
            (thread standard-listener)
            (make-thread fn :name (name standard-listener))))))

(defun %listener-ready-socket (listener)
  (let* ((connections (with-lock-held ((lock listener)) (connections listener)))
         (sockets (mapcar #'socket connections)))
    (first (wait-until (wait-for-input sockets :timeout nil :ready-only t)))))

(defun %listener-loop (listener)
  (with-restartability (listener)
    (handler-case
        (let* ((socket (%listener-ready-socket listener))
               (connection (owner socket))
               (command (connection-receive connection)))
          (when command
            (funcall (handler listener) connection command)))
      (stream-error (e)
        (%listener-error listener e)))))

(defun %listener-error (listener condition)
  (let* ((connections (with-lock-held ((lock listener)) (connections listener)))
         (stream (stream-error-stream condition))
         (predicate (lambda (x) (eq stream (socket-stream (socket x)))))
         (connection (find-if predicate connections)))
    (when connection
      (kill connection)
      (with-lock-held ((lock listener))
        (setf (connections listener)
              (remove connection (connections listener) :count 1))))))

(defmethod deadp ((listener standard-listener))
  (not (thread-alive-p (thread listener))))

(defmethod kill ((listener standard-listener))
  (unless (eq (current-thread) (thread listener))
    (destroy-thread (thread listener)))
  (unless (deadp listener)
    (kill (notifier-connection listener))
    (with-lock-held ((lock listener))
      (mapc #'kill (connections listener))
      (setf (connections listener) ())))
  (values))
