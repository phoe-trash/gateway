;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem.lisp

(in-package #:gateway)

(defclass standard-gem (gem)
  ((%owner :accessor owner
	   :initarg :owner
	   :initform (error "No OWNER provided."))
   (%thread :accessor thread)))

(defconstructor (standard-gem)
  (check-type (owner standard-gem) (or crown jewel))
  (setf (thread standard-gem) (%make-gem-thread standard-gem)))

(defmethod kill ((gem standard-gem))
  (deletef (gems (owner gem)) gem)
  (unless (eq (current-thread) (thread gem))
    (destroy-thread (thread gem)))
  (values))

(defun %make-gem-thread (gem)
  (let ((owner (owner gem)))
    (make-thread (lambda () (%gem gem))
		 :name (format nil "Gateway - gem for ~S" (type-of owner)))))

(defun %gem (gem)
  (check-type gem gem)
  (format t "[~~] Gem: starting.~%")
  (unwind-protect
       (%gem-function gem)
    (kill gem)
    (format t "[!] Gem: thread killed.~%")))

(defun %gem-function (gem)
  (restart-case
      (typecase (owner gem)
        (crown (loop (%gem-crown-loop gem)))
        (jewel (loop (%gem-jewel-loop gem))))
    (retry ()
      :report "Abort the current iteration and send the gem back to its loop."
      (format t "[!] Gem: restart invoked.~%")
      (%gem-function gem))))

(macrolet
    ((clean (lock-fn conn-fn type)
       `(let ((crown (owner gem)))
          (with-lock-held ((,lock-fn crown))
            (let* ((to-cleanup (remove-if #'alivep (,conn-fn crown)))
		   (size (length to-cleanup)))
              (when to-cleanup
                (format t "[~~] Gem: cleaning ~D dead ~A-connections.~%" size ,type)
                (mapc (lambda (x) (deletef (,conn-fn crown) x)) to-cleanup))))))
     (n-clean () '(clean n-lock n-connections :n))
     (e-clean () '(clean e-lock e-connections :e))
     (i-clean () '(clean i-lock i-connections :i)))
  (defun %gem-crown-loop (gem)
    (or (%gem-crown-queue gem)
	(i-clean)
	(e-clean)
	(n-clean)
	(sleep 0.1))))

(defun %gem-crown-queue (gem)
  (let* ((crown (owner gem))
         (queue (event-queue crown))
         (inner-queue (slot-value queue 'jpl-queues::queue))
         (lock (slot-value queue 'jpl-queues::lock)))
    (with-lock-held (lock)
      (unless (empty? inner-queue)
        (let ((entry (dequeue inner-queue)))
          (format t "[.] Gem: on the queue: ~S~%" entry)
          (%gem-crown-parse-entry crown entry))
        t))))

(defun %gem-crown-parse-entry (crown entry)
  (destructuring-bind (type connection command) entry
    (check-type crown crown)
    (check-type connection connection)
    (check-type command cons)
    (let ((hash-map (%gem-crown-hash-table type)))
      (%%gem-crown-parse-entry crown connection command hash-map type))))

(defun %gem-crown-hash-table (type)
  (case type
    (:n *gem-n-handlers*)
    (:e *gem-e-handlers*)
    (:i *gem-i-handlers*)
    (t (error "Bad type: ~S" type))))

(defun %%gem-crown-parse-entry (crown connection command hash-map type)
  (flet ((err (error-type)
           (%gem-crown-parse-error error-type connection command type)))
    (destructuring-bind (command-word . arguments) command
      (multiple-value-bind (function function-found-p)
	  (gethash (symbol-name command-word) hash-map)
	(let ((args (list* function crown connection arguments)))
	  (cond ((not function-found-p)
		 (err :unknown-function))
		((not (apply #'verify-arguments args))
		 (err :malformed-arguments))
		(t (format t "[.] Applying function on command ~S.~%" command)
		   (apply function crown connection arguments))))))))

(defun %gem-crown-parse-error (error-type connection command type)
  (format t "[!] Gem: ~A error on ~S, command ~S.~%" error-type connection command)
  (send connection `(error ,error-type ,command))
  (when (eq type :n)
    (format t "[!] Gem: killing connection because of error.~%")
    (kill connection)))



(defun %gem-jewel-loop (gem)
  (declare (ignore gem))
  (error "Not implemented yet."))
