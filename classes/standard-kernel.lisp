;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes/standard-kernel.lisp

(in-package :gateway/impl)

(defclass standard-kernel (kernel)
  ((%kernel :accessor %kernel)
   (%channel :accessor channel)
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

;;; TODO define-print for all classes

(define-constructor (standard-kernel threads)
  (let* ((threads (or threads (cl-cpus:get-number-of-processors)))
         (name "Gateway - Kernel, ~D threads" cpus))
    (setf (name standard-kernel) name
          (%kernel standard-kernel)
          (lparallel:make-kernel threads
                                 :name (cat name " (lparallel kernel)")))
    (let ((lparallel:*kernel* (%kernel standard-kernel)))
      (setf (channel standard-kernel)
            (lparallel:make-channel)))))

(defmethod deadp ((kernel standard-kernel))
  (not (lparallel.kernel::alivep (%kernel kernel))))

(defmethod kill ((kernel standard-kernel))
  (let ((lparallel:*kernel* (%kernel kernel)))
    (lparallel:end-kernel :wait t))
  (values))

(defmethod enqueue ((kernel standard-kernel) message)
  ;; TODO cost of one thread waiting is less than cost of each worker polling.
  ;;      consider creating a separate thread for that instead of polling on
  ;;      each enqueue
  (remove-results kernel)
  (lparallel:submit-task (channel kernel) (handler kernel) message)
  t)

(defun remove-results (kernel)
  (loop for (result is) = (multiple-value-list
                           (lparallel:try-receive-result (channel kernel)))
        while is))

;;; TESTS

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
  1. "Create a handler that atomically (with a lock) increases a variable that ~
is initially 0."
  2. "Create a kernel."
  3. "Prepare a shuffled list of integers from 1 to 100."
  :act
  4. "Submit 100 tasks to the kernel which increase the variable by an integer."
  :assert
  5. "Assert the variable is = to 5050.")

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
    #5?(is (wait () (with-lock-held (lock) (= var 5050))))))
