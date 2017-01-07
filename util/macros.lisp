;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

;;;; DEFTEST
(defmacro deftest (name &body body)
  `(progn
     (1am:test ,name
       (bt:join-thread (make-thread (lambda () (note "[T] ~A~%" ',name))))
       ,@body)))

;;;; DEFINE-PROTOCOL-CLASS
(defmacro define-protocol-class (name super-classes &optional slots &rest options)
  (let* ((sym-name (symbol-name name))
         (protocol-predicate
           (intern (concatenate 'string sym-name
                                (if (find #\- sym-name) "-" "") (symbol-name '#:p))))
         (predicate-docstring
           (concatenate 'string "Returns T if object is of class " sym-name
                        ", otherwise returns NIL.")))
    `(progn
       (defclass ,name ,super-classes ,slots ,@options)
       (let ((the-class (find-class ',name)))
         (setf (documentation the-class 'type) "Gateway protocol class")
         (defmethod initialize-instance :after ((object ,name) &key &allow-other-keys)
           (when (eq (class-of object) the-class)
             (error "~S is a protocol class and thus can't be instantiated."
                    ',name))))
       (defgeneric ,protocol-predicate (object)
         (:method ((object t)) nil)
         (:method ((object ,name)) t)
         (:documentation ,predicate-docstring))
       ',name)))

;;;; DEFPROTOCOL
(defmacro defprotocol (protocol-name (&optional class-name class-args class-slots
                                      &body class-options)
                       &body body)
  (declare (ignore protocol-name))
  `(progn
     ,(when class-name
        `(define-protocol-class ,class-name ,class-args ,class-slots ,@class-options))
     ,@body))

;;;; DEFCONSTRUCTOR
(defmacro defconstructor ((class . keys) &body body)
  `(defmethod initialize-instance :after ((,class ,class)
                                          &key ,@keys &allow-other-keys)
     ,@body))

;;;; DEFPRINT
(defmacro defprint (object &body body)
  `(defmethod print-object ((obj ,object) stream)
     ,@body))

;;;; WAIT
(defmacro wait ((&optional (timeout 2) (step 0.01)) &body body)
  (with-gensyms (begin-time end-time temp)
    `(let* ((,begin-time (get-internal-real-time))
            (,end-time (+ ,begin-time (* ,timeout internal-time-units-per-second))))
       (loop
         (let (,temp)
           (cond ((progn (setf ,temp (progn ,@body))
                         ,temp)
                  (return ,temp))
                 ((> (get-internal-real-time) ,end-time)
                  (return nil))
                 (t
                  (sleep ,step))))))))

;;;; WAIT-UNTIL
(defmacro wait-until (form &optional (step 0.01))
  (with-gensyms (result)
    `(loop for ,result = ,form
           if ,result return ,result
             else (sleep ,step))))

;;;; FINALIZED-LET
(defmacro finalized-let* ((&rest bindings) &body body)
  (if bindings
      `(let (,(first (first bindings)))
         (unwind-protect
              (progn (setf ,(first (first bindings))
                           ,(second (first bindings)))
                     (finalized-let* ,(rest bindings) ,@body))
           (when ,(first (first bindings))
             (progn ,@(cddr (first bindings))))))
      `(progn ,@body)))

;;;; WITH-THREAD-HANDLERS
(defmacro with-thread-handlers ((symbol) &body body)
  `(labels
       ((%loop-1 (,symbol)
          (note "[~~] ~A: starting.~%" (name ,symbol))
          (unwind-protect
               (%loop-2 ,symbol)
            (kill ,symbol)
            (note "[!] ~A: killed.~%" (name ,symbol))))
        (%loop-2 (,symbol)
          (restart-case
              (loop (%loop-3 ,symbol))
            (retry ()
              :report %report
              (note "[!] ~A: restarted.~%" (name ,symbol))
              (%loop-2 ,symbol))))
        (%loop-3 (,symbol)
          ,@body)
        (%report (stream)
          (fformat stream
                   "Abort the current iteration and send the ~A back to its loop."
                   (string-downcase (string ',symbol)))))
     (%loop-1 ,symbol)))

;;;; WITH-CROWN-AND-CONNECTIONS
(defmacro with-crown-and-connections (crown-var n-connections i-connections &body body)
  (with-gensyms (n-host n-port i-host i-port)
    `(multiple-value-bind (,crown-var ,n-host ,n-port ,i-host ,i-port)
         (%make-crown-with-listed-ports)
       (declare (ignore ,@(unless n-connections (list n-host n-port))
                        ,@(unless i-connections (list i-host i-port))))
       (unwind-protect
            (finalized-let*
                (,@(%with-crown-and-connections-list n-connections n-host n-port)
                 ,@(%with-crown-and-connections-list i-connections i-host i-port))
              ,@body)
         (kill ,crown-var)))))

(defun %with-crown-and-connections-list (connections host port)
  (flet ((generate (x) `(,x (%make-connection ,host ,port) (kill ,x))))
    (mapcar #'generate connections)))

(defun %reporting-debugger-hook (condition value)
  (declare (ignore value))
  (note "[!] DEBUGGER ENTERED:~%~A~%" condition)
  (sb-debug:print-backtrace)
  (let ((*debugger-hook* #'swank:swank-debugger-hook))
    (swank:swank-debugger-hook condition *debugger-hook*)))

(setf *debugger-hook* #'%reporting-debugger-hook)

(setf *default-special-bindings*
      (list (cons '*debugger-hook* #'%reporting-debugger-hook)))

;;;; DEFCONFIG / WITH-CLEAN-CONFIG
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar *config-vars* nil)
;;   (defvar *cache-vars* nil)
;;   (defmacro defconfig (var val &key cache doc)
;;     `(progn (pushnew (list ',var ',val) *config-vars* :test #'equal)
;; 	    (defvar ,var ,val ,doc)
;; 	    ,@(when cache
;; 		`((pushnew (list ',cache ',var) *cache-vars*)
;; 		  (setf (gethash ,cache *cache-list*) ,var)))))
;;   (defmacro with-clean-config (&body body)
;;     `(let ,*config-vars*
;;        ;; TODO: use PROGV for dynamic binding
;;        ;; reconstruct *CACHE-LIST*
;;        (mapc (lambda (x) (setf (gethash (first x) *cache-list*) (second x)))
;; 	     (list ,@(mapcar (lambda (x) `(list ',(first x) ,(second x))) *cache-vars*)))
;;        ,@body)))

;;;; WITH-CONNECTIONS
;; (defmacro with-connections (connections &body body)
;;   `(let* ,connections
;;      (unwind-protect
;;           ,@body
;;        (mapcar #'kill (list ,@(mapcar #'first connections))))))

;;;; TESTING
;; (defun begin-tests ()
;;   (make-thread (lambda () (format t "~%[~~] Begin running tests.~%"))))

;; (defun finish-tests ()
;;   (make-thread (lambda () (format t "[~~] Finished running tests.~%"))))

;;;; DEFCOMMAND
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun %defcommand-map (args)
;;     (when args
;;       (destructuring-bind (first . rest) args
;;         (let ((elt (ecase first
;;                      (:n '*gem-n-handlers*)
;;                      (:e '*gem-e-handlers*)
;;                      (:i '*gem-i-handlers*))))
;;           (cons elt (%defcommand-map rest))))))
;;   (defmacro defcommand (command types (crown-var connection-var &rest arguments)
;;                         &body body)
;;     `(let* ((command-name (symbol-name ,command))
;;             (function (compile nil (lambda (,crown-var ,connection-var ,@arguments) ,@body))))
;;        (flet ((hash-push (hash-table) (setf (gethash command-name hash-table) function)))
;;          (mapcar #'hash-push (list ,@(%defcommand-map types)))))))
