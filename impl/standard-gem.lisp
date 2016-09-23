;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun %gem-crown-loop (gem)
  (or (%gem-crown-queue gem)
      (%gem-crown-clean-i-conn gem)
      (%gem-crown-clean-e-conn gem)
      (%gem-crown-clean-n-conn gem) 
      (sleep 0.1)))

(macrolet
    ((clean (lock-fn conn-fn type)
       `(let ((crown (owner gem)))
          (with-lock-held ((,lock-fn crown))
            (let ((to-cleanup (remove-if #'alivep (,conn-fn crown))))
              (when to-cleanup
                (format t "[~~] Gem: cleaning ~D dead ~A-connections.~%" (length to-cleanup) ,type)
                (mapc (lambda (x) (deletef (,conn-fn crown) x)) to-cleanup)))))))
  (defun %gem-crown-clean-n-conn (gem)
    (clean n-lock n-connections :n))
  (defun %gem-crown-clean-e-conn (gem)
    (clean e-lock e-connections :e))
  (defun %gem-crown-clean-i-conn (gem)
    (clean i-lock i-connections :i)))

(defun %gem-crown-queue (gem)
  (let* ((crown (owner gem))
         (queue (event-queue crown))
         (inner-queue (slot-value queue 'jpl-queues::queue))
         (lock (slot-value queue 'jpl-queues::lock)))
    (with-lock-held (lock)
      (unless (empty? inner-queue)
        (let ((entry (dequeue inner-queue)))
          (format t "[.] Gem: on the queue: ~S~%" entry)
          (parse-entry crown entry))
        t))))

(defun parse-entry (crown entry)
  (destructuring-bind (type connection command) entry
    (check-type crown crown)
    (check-type connection connection)
    (check-type command cons)
    (let 
	;;(format t "PARSE-ENTRY: ~S~%~S ~S~%" command type hash-map)
	(%parse-entry crown connection command type))))

(defun %parse-entry (crown connection command type)
  (let ((hash-map (case type
		    (:n *gem-n-handlers*)
		    (:e *gem-e-handlers*)
		    (:i *gem-i-handlers*)
		    (t (error "Bad type: ~S" type)))))
    (destructuring-bind (command-word . arguments) command
      (multiple-value-bind (function function-found-p) 
	  (gethash (symbol-name command-word) hash-map)
	(cond ((not function-found-p)
	       (%parse-entry-error :unknown-function connection type command))
	      ((not (apply #'verify-arguments function 
			   (list* crown connection arguments)))
	       (%parse-entry-error :malformed-arguments connection type command))
	      (t
	       (format t "[.] Applying function on command ~S.~%" command)
	       (apply function crown connection arguments)))))))

(defun %parse-entry-error (error-type connection type command)
  (format t "[!] ~A error on connection ~S, command ~S.~%"
	  error-type connection command)  
  (send connection `(error ,error-type ,command))
  (when (eq type :n) (kill connection)))

;; (defun parse-n-entry (crown connection command)
;;   (check-type crown crown)
;;   (check-type connection connection)
;;   (check-type command cons)
;;   (cond (t
;; 	 (progn
;; 	   (format t "[~~] Gem: killing N-connection.~%")
;; 	   (send connection (list 'error 'wrong-greeting command)) 
;; 	   (with-lock-held ((n-lock crown))
;; 	     (deletef (n-connections crown) connection))
;; 	   (kill connection)))))

;; (defun parse-e-entry (crown connection command)
;;   (check-type crown crown)
;;   (check-type connection connection)
;;   (check-type command cons)
;;   nil)

;; (defun parse-i-entry (crown connection command)
;;   (check-type crown crown)
;;   (check-type connection connection)
;;   (check-type command cons)
;;   nil)

;; (defun %gem-crown-i-conn (gem)
;;   (let ((crown (owner gem)))
;;     (let ((connection (with-lock-held ((i-lock crown)) (find-if #'readyp (i-connections crown)))))
;;       (when connection
;;         (let ((object (receive connection)))
;;           (when object
;;             (format t "[.] Gem: I-received: ~S~%" object)))
;;         t))))

;; (defun %gem-crown-e-conn (gem)
;;   (let ((crown (owner gem)))
;;     (let ((connection (with-lock-held ((e-lock crown)) (find-if #'readyp (e-connections crown)))))
;;       (when connection
;;         (let ((object (receive connection)))
;;           (when object
;;             (format t "[.] Gem: E-received: ~S~%" object)))
;;         t))))

;; (defun %gem-crown-n-conn (gem)
;;   (let ((crown (owner gem)))
;;     (let ((connection (with-lock-held ((n-lock crown)) (find-if #'readyp (n-connections crown)))))
;;       (when connection
;;         (let ((object (receive connection)))
;;           (when object
;;             (format t "[~~] Gem: N-received: ~S~%" object)
;;             (with-lock-held ((n-lock crown))
;;               (deletef (n-connections crown) connection))
;;             (cond ((and (consp object)
;;                         (string= (first object) :open)
;;                         (string= (second object) :gateway))
;;                    (format t "[~~] Gem: accepting E-connection.~%")
;;                    (with-lock-held ((e-lock crown))
;;                      (pushnew connection (e-connections crown))))
;;                   (t
;;                    (format t "[~~] Gem: killing N-connection.~%")
;;                    (send connection (list 'error 'wrong-greeting object))
;;                    (kill connection))))
;;           t)))))

(defun %gem-jewel-loop (gem)
  (declare (ignore gem))
  (error "Not implemented yet."))
