;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-kernel.lisp

(in-package :gateway/impl)

(in-readtable protest)

(defclass standard-kernel (kernel)
  ((%kernel :accessor %kernel)
   (%channel :accessor channel)
   (%cleaner :accessor cleaner)
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must provide a handler function."))
   (%name :accessor name))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class KERNEL.

This kernel contains a lparallel kernel. The kernel workers each call the ~
handler function with the message as its argument. That function executes a ~
single iteration of the kernel's job and is not meant to loop.

TODO these below lines belong to the framework, not the class

The default implementation of the handler function is the exported function ~
#'STANDARD-KERNEL-OPERATION.

The messages are expected to be in form (CONNECTION COMMAND . REST), where ~
CONNECTION is a connection object from which MESSAGE came; REST is ignored ~
and reserved for future use.")))

(define-print (standard-kernel stream)
  (if (alivep standard-kernel)
      (format stream "(~D workers, ALIVE)"
              (worker-count standard-kernel))
      (format stream "(DEAD)")))

(defun worker-count (kernel)
  (check-type kernel standard-kernel)
  (let ((lparallel:*kernel* (%kernel kernel)))
    (lparallel:kernel-worker-count)))

(define-constructor (standard-kernel threads)
  (let* ((threads (or threads
                      (config :kernel-threads)
                      (cl-cpus:get-number-of-processors)))
         (name "Gateway - Kernel, ~D threads" cpus))
    (setf (name standard-kernel) name
          (%kernel standard-kernel)
          (lparallel:make-kernel threads
                                 :name (cat name " (lparallel kernel)")))
    (let ((lparallel:*kernel* (%kernel standard-kernel)))
      (setf (channel standard-kernel) (lparallel:make-channel)
            (cleaner standard-kernel)
            (make-thread
             (lambda ()
               (loop (lparallel:receive-result (channel standard-kernel))))
             :name "Gateway - Kernel cleaner thread")))))

(defmethod deadp ((kernel standard-kernel))
  (not (lparallel.kernel::alivep (%kernel kernel))))

(defmethod kill ((kernel standard-kernel))
  (let ((lparallel:*kernel* (%kernel kernel)))
    (lparallel:end-kernel :wait t))
  (destroy-thread (cleaner kernel))
  (values))

(defmethod enqueue ((kernel standard-kernel) message)
  (lparallel:submit-task (channel kernel) (handler kernel) message)
  t)

;;; TESTS

;; TODO separate tests from code

(define-test-case standard-kernel-death
    (:description "Test of KILLABLE protocol for STANDARD-LISTENER."
     :tags (:protocol :killable :kernel)
     :type :protocol)
  :arrange
  1 "Create a kernel."
  2 "Assert the kernel is alive."
  :act
  3 "Kill the kernel."
  :assert
  4 "Assert the kernel is dead.")

(define-test standard-kernel-death
  (let* ((kernel #1?(make-instance 'standard-kernel :handler (constantly nil))))
    #2?(is (alivep kernel))
    #3?(kill kernel)
    #4?(is (wait () (deadp kernel)))))

(define-test-case standard-kernel
    (:description "Test the kernel's enqueueing functionality."
     :tags (:implementation :kernel)
     :type :implementation)
  :arrange
  1 "Create a handler that atomically (with a lock) increases a variable that ~
is initially 0."
  2 "Create a kernel."
  3 "Prepare a shuffled list of integers from 1 to 100."
  :act
  4 "Submit 100 tasks to the kernel which increase the variable by an integer."
  :assert
  5 "Assert the variable is = to 5050."
  6 "Assert no more results are available in the channel.")

(define-test standard-kernel
  (finalized-let*
      ((var 0)
       (lock (make-lock "STANDARD-KERNEL test lock"))
       (handler #1?(lambda (x) (with-lock-held (lock) (incf var x))))
       (kernel #2?(make-instance 'standard-kernel :handler handler)
               (kill kernel))
       (tasks #3?(shuffle (iota 100 :start 1))))
    #4?(dolist (i tasks)
         (is (enqueue kernel i)))
    #5?(is (wait () (with-lock-held (lock) (= var 5050))))
    (sleep 0.1)
    #6?(is (wait () (equal '(nil nil)
                           (multiple-value-list
                            (lparallel.queue:peek-queue
                             (lparallel.kernel::channel-queue
                              (channel kernel)))))))))
