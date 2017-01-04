;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem.lisp

(in-package #:gateway)

(defclass standard-gem (gem)
  ((%thread :accessor thread)
   (%name :accessor name)
   (%push-on-error-p :accessor push-on-error-p
                     :initarg :push-on-error-p
                     :initform nil)
   (%getter :accessor getter
            :initarg :getter
            :initform (error "Must define a getter function."))
   (%pusher :accessor pusher
            :initarg :pusher
            :initform (error "Must define a pusher function."))
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function."))))

(defconstructor (standard-gem)
  (let* ((name "Gateway - Gem")
         (fn (curry #'%gem-loop standard-gem)))
    (setf (name standard-gem) name
          (thread standard-gem) (make-thread fn :name name))))

(defun %gem-loop (gem)
  (with-thread-handlers (gem)
    (let ((message (funcall (getter gem))))
      (handler-case
          (funcall (handler gem) message)
        (error (e)
          (format t "[!] Gem: ~A~%" e)
          (if (push-on-error-p gem)
              (funcall (pusher gem) message)
              (error e)))))))

(defmethod alivep ((gem standard-gem))
  (thread-alive-p (thread gem)))

(defmethod kill ((gem standard-gem))
  (unless (eq (current-thread) (thread gem))
    (destroy-thread (thread gem)))
  (values))

(defun %make-gem (getter pusher handler push-on-error-p)
  (make-instance 'standard-gem :getter getter :pusher pusher :handler handler
                               :push-on-error-p push-on-error-p))



(deftest test-standard-gem-death
  (let* ((getter (lambda () (loop (sleep 1))))
         (pusher (lambda (x) (declare (ignore x))))
         (gem (%make-gem getter pusher pusher nil)))
    (is (alivep gem))
    (kill gem)
    (is (wait () (deadp gem)))))

(deftest test-standard-gem
  (finalized-let*
      ((data '(0 1 2 3 4 5 6 7 8 9))
       (accepted-data ())
       (getter (lambda () (if data (pop data) (loop (sleep 1)))))
       (pusher (lambda (x) (declare (ignore x))))
       (handler (lambda (x) (push x accepted-data)))
       (gem (%make-gem getter pusher handler nil)
            (kill gem)))
    (is (wait () (equal accepted-data '(9 8 7 6 5 4 3 2 1 0))))
    (is (null data))))

(deftest test-standard-gem-error-push
  (finalized-let*
      ((data '(0 1 2 3 4 5 6 7 8 9))
       (accepted-data ())
       (rejected-data ())
       (error-msg "Invoking test error.")
       (getter (lambda () (if data (pop data) (loop (sleep 1)))))
       (pusher (lambda (x) (push x rejected-data)))
       (handler (lambda (x) (if (oddp x) (error error-msg) (push x accepted-data))))
       (gem (%make-gem getter pusher handler t)
            (kill gem)))
    (is (wait () (equal rejected-data '(9 7 5 3 1))))
    (is (equal accepted-data '(8 6 4 2 0)))
    (is (null data))))

