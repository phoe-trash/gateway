;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem.lisp

(in-package #:gateway)

(defclass standard-gem (gem)
  ((%parent :accessor parent
	    :initarg :parent
	    :initform (error "No PARENT provided."))
   (%thread :accessor thread)))

(defconstructor (standard-gem)
  (check-type (parent standard-gem) (or crown jewel))
  (pushnew standard-gem (gems (parent standard-gem)))
  (setf (thread standard-gem) (%make-gem-thread standard-gem)))

(defmethod kill ((gem standard-gem))
  (deletef (gems (parent gem)) gem)
  (unless (eq (current-thread) (thread gem))
    (destroy-thread (thread gem)))
  (values))

(defun %make-gem-thread (gem)
  (let ((parent (parent gem)))
    (make-thread (lambda () (%gem gem))
		 :name (format nil "Gateway - gem for ~S" (type-of parent)))))

(defun %gem (gem)
  (check-type gem gem)
  (format t "[~~] Gem: starting.~%")
  (unwind-protect
       (%gem-function gem)
    (kill gem)
    (format t "[!] Gem: thread killed.~%")))

(defun %gem-function (gem)
  (restart-case
      (typecase (parent gem)
        (crown (loop (%gem-crown-loop gem)))
        (jewel (loop (%gem-jewel-loop gem))))
    (retry ()
      :report "Abort the current iteration and send the gem back to its loop."
      (format t "[!] Gem: restart invoked.~%")
      (%gem-function gem))))

(defun %gem-crown-loop (gem)
  (or (%gem-crown-queue gem)
      (progn
        (%gem-crown-clean-i-conn gem)
        (%gem-crown-i-conn gem))
      (progn
        (%gem-crown-clean-e-conn gem)
        (%gem-crown-e-conn gem))
      (progn
        (%gem-crown-clean-n-conn gem)
        (%gem-crown-n-conn gem))
      (sleep 0.01)))

(defun %gem-crown-queue (gem)
  (let* ((crown (parent gem))
         (queue (event-queue crown)))
    (declare (ignore queue))
    nil))

(defun %gem-crown-clean-i-conn (gem)
  (let ((crown (parent gem)))
    (with-lock-held ((i-lock crown))
      (let ((to-cleanup (remove-if #'alivep (i-connections crown))))
        (when to-cleanup
          (format t "[~~] Gem: cleaning I-connections.~%")
          (mapc (lambda (x) (deletef (i-connections crown) x))
                to-cleanup)
          t)))))

(defun %gem-crown-clean-e-conn (gem)
  (let ((crown (parent gem)))
    (with-lock-held ((e-lock crown))
      (let ((to-cleanup (remove-if #'alivep (e-connections crown))))
        (when to-cleanup
          (format t "[~~] Gem: cleaning E-connections.~%")
          (mapc (lambda (x) (deletef (e-connections crown) x))
                to-cleanup)
          t)))))

(defun %gem-crown-clean-n-conn (gem)
  (let ((crown (parent gem)))
    (with-lock-held ((n-lock crown))
      (let ((to-cleanup (remove-if #'alivep (n-connections crown))))
        (when to-cleanup
          (format t "[~~] Gem: cleaning N-connections.~%")
          (mapc (lambda (x) (deletef (n-connections crown) x))
                to-cleanup)
          t)))))

(defun %gem-crown-i-conn (gem)
  (let ((crown (parent gem)))
    (let ((connection (with-lock-held ((i-lock crown)) (find-if #'readyp (i-connections crown)))))
      (when connection
        (let ((object (receive connection)))
          (when object
            (format t "[.] Gem: I-received: ~S~%" object)))
        t))))

(defun %gem-crown-e-conn (gem)
  (let ((crown (parent gem)))
    (let ((connection (with-lock-held ((e-lock crown)) (find-if #'readyp (e-connections crown)))))
      (when connection
        (let ((object (receive connection)))
          (when object
            (format t "[.] Gem: E-received: ~S~%" object)))
        t))))

(defun %gem-crown-n-conn (gem)
  (let ((crown (parent gem)))
    (let ((connection (with-lock-held ((n-lock crown)) (find-if #'readyp (n-connections crown)))))
      (when connection
        (let ((object (receive connection)))
          (when object
            (format t "[~~] Gem: N-received: ~S~%" object)
            (with-lock-held ((n-lock crown))
              (deletef (n-connections crown) connection))
            (cond ((and (consp object)
                        (string= (first object) :open)
                        (string= (second object) :gateway))
                   (format t "[~~] Gem: accepting E-connection.~%")
                   (with-lock-held ((e-lock crown))
                     (pushnew connection (e-connections crown))))
                  (t
                   (format t "[~~] Gem: killing N-connection.~%")
                   (send connection (list 'error 'wrong-greeting object))
                   (kill connection))))
          t)))))

(defun %gem-jewel-loop (gem)
  (declare (ignore gem))
  (error "Not implemented yet."))
