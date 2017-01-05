;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-crown.lisp

(in-package #:gateway)

(defclass standard-crown (crown)
  ((%library :accessor library)
   (%queue :accessor queue)
   (%timer :accessor timer)
   (%gems :accessor gems :initform ())
   (%operations :accessor operations :initform ())
   (%n-acceptor :accessor n-acceptor)
   (%n-connections :accessor n-connections :initform ())
   (%n-lock :accessor n-lock :initform (%crown-lock "N"))
   (%n-listener :accessor n-listener)
   (%e-connections :accessor e-connections :initform ())
   (%e-lock :accessor e-lock :initform (%crown-lock "E"))
   (%e-listener :accessor e-listener)
   (%i-acceptor :accessor i-acceptor)
   (%i-connections :accessor i-connections :initform ())
   (%i-lock :accessor i-lock :initform (%crown-lock "I"))
   (%i-listener :accessor i-listener)))

(defconstructor (standard-crown new
                                (n-host "127.0.0.1") (n-port 0)
                                (i-host "127.0.0.1") (i-port 0))
  (if new
      (%crown-constructor-new standard-crown n-host n-port i-host i-port)
      (error "Constructing from existing data not implemented yet.")))

(defun %crown-constructor-new (crown n-host n-port i-host i-port)
  (destructuring-bind (n-getter e-getter i-getter
                       n-pusher e-pusher i-pusher
                       data-getter data-pusher data-handler
                       sexp-data-pusher)
      (%crown-constructor-lambdas crown)
    (setf  (library crown) (make-instance 'standard-library)
           (queue crown) (make-queue)
           (n-acceptor crown) (%make-acceptor n-host n-port n-pusher)
           (i-acceptor crown) (%make-acceptor i-host i-port i-pusher)
           (n-listener crown) (%make-listener n-getter n-pusher sexp-data-pusher)
           (e-listener crown) (%make-listener e-getter e-pusher sexp-data-pusher)
           (i-listener crown) (%make-listener i-getter i-pusher sexp-data-pusher)
           (operations crown) (%crown-standard-operations crown)
           (timer crown) (%make-timer (operations crown) 200.0 data-pusher)
           (gems crown) (list (%make-gem data-getter data-pusher data-handler nil)))))

(defun %crown-constructor-lambdas (crown)
  (list
   (lambda () (with-lock-held ((n-lock crown)) (n-connections crown)))
   (lambda () (with-lock-held ((e-lock crown)) (e-connections crown)))
   (lambda () (with-lock-held ((i-lock crown)) (i-connections crown)))
   (lambda (x) (with-lock-held ((n-lock crown)) (push x (n-connections crown))))
   (lambda (x) (with-lock-held ((e-lock crown)) (push x (e-connections crown))))
   (lambda (x) (with-lock-held ((i-lock crown)) (push x (i-connections crown))))
   (lambda () (pop-queue (queue crown)))
   (lambda (x) (push-queue x (queue crown)))
   (lambda (x) (apply #'execute-operation x))
   (lambda (connection command)
     (push-queue `(execute-command :crown ,crown :command ,command :connection ,connection)
                 (queue crown)))))

(defun %crown-standard-operations (crown)
  `((clean-connections :lock ,(n-lock crown)
                       :getter ,(curry #'n-connections crown)
                       :setter ,(lambda (x) (setf (n-connections crown) x)))
    (clean-connections :lock ,(e-lock crown)
                       :getter ,(curry #'e-connections crown)
                       :setter ,(lambda (x) (setf (e-connections crown) x)))
    (clean-connections :lock ,(i-lock crown)
                       :getter ,(curry #'i-connections crown)
                       :setter ,(lambda (x) (setf (i-connections crown) x)))))

(defun %crown-lock (string)
  (make-lock (format nil "Gateway - Crown ~A-lock" string)))

(defmethod alivep ((crown standard-crown))
  (some #'alivep (list* (n-acceptor crown) (n-listener crown) (e-listener crown)
                        (i-acceptor crown) (i-listener crown) (timer crown)
                        (gems crown))))

(defmethod kill ((crown standard-crown))
  (let ((elements (list (n-acceptor crown) (n-listener crown) (e-listener crown)
                        (i-acceptor crown) (i-listener crown) (timer crown))))
    (mapc #'kill elements)
    (mapc #'kill (gems crown)))
  (values))

(defun %make-crown-with-listed-ports ()
  (let* ((crown (make-instance 'standard-crown :new t))
         (n-socket (socket (n-acceptor crown)))
         (i-socket (socket (i-acceptor crown))))
    (values crown
            (%host-to-string (get-local-address n-socket))
            (get-local-port n-socket)
            (%host-to-string (get-local-address i-socket))
            (get-local-port i-socket))))

(defun %make-crown ()
  (make-instance 'standard-crown :new t))



(deftest test-standard-crown-death
  (let* ((crown (make-instance 'standard-crown :new t))
         (elements (list* (n-acceptor crown) (n-listener crown) (e-listener crown)
                          (i-acceptor crown) (i-listener crown) (timer crown)
                          (gems crown))))
    (kill crown)
    (is (wait () (deadp crown)))
    (is (wait () (every #'deadp elements)))))

(deftest test-standard-crown-single-connections
  (flet ((connect (host port) (%make-connection host port)))
    (multiple-value-bind (crown n-host n-port i-host i-port)
        (%make-crown-with-listed-ports)
      (unwind-protect
           (progn
             (is (wait () (= 1 (length (n-connections crown)))))
             (is (wait () (= 1 (length (e-connections crown)))))
             (is (wait () (= 1 (length (i-connections crown)))))
             (finalized-let* ((connection (connect n-host n-port)
                                          (kill connection)))
               (is (wait () (= 2 (length (n-connections crown))))))
             (is (wait () (= 1 (length (n-connections crown)))))
             (finalized-let* ((connection (connect i-host i-port)
                                          (kill connection)))
               (is (wait () (= 2 (length (i-connections crown))))))
             (is (wait () (= 1 (length (i-connections crown))))))
        (kill crown)))))

(deftest test-standard-crown-multi-connection
  (flet ((connect (host port) (%make-connection host port)))
    (multiple-value-bind (crown n-host n-port i-host i-port)
        (%make-crown-with-listed-ports)
      (declare (ignore i-host i-port))
      (unwind-protect
           (finalized-let* ((connection-1 (connect n-host n-port)
                                          (kill connection-1))
                            (connection-2 (connect n-host n-port)
                                          (kill connection-2))
                            (connection-3 (connect n-host n-port)
                                          (kill connection-3)))
             (is (wait () (= 4 (length (n-connections crown))))))
        (is (wait () (= 1 (length (n-connections crown)))))
        (kill crown)))))
