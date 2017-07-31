;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/macros.lisp

(in-package #:gateway/utils)

(defmacro define-constructor ((class . keys) &body body)
  "Defines an INITIALIZE-INSTANCE :AFTER method on the given object."
  `(defmethod initialize-instance :after ((,class ,class)
                                          &key ,@keys &allow-other-keys)
     ,@body))

(defmacro define-print (object &body body)
  "Defines a PRINT-OBJECT method on the given object."
  `(defmethod print-object ((,object ,object) stream)
     ,@body))

;;;; WAIT
(defmacro wait ((&optional (timeout 2) (step 0.01)) &body body)
  "Evaluates BODY each STEP seconds until it evaluates to true, at which point
it returns the value of BODY, or until TIMEOUT seconds pass, at which point it
returns NIL."
  (with-gensyms (begin-time end-time temp)
    `(let* ((units internal-time-units-per-second)
            (,begin-time (get-internal-real-time))
            (,end-time (+ ,begin-time (* ,timeout units))))
       (loop
         (let (,temp)
           (cond ((progn (setf ,temp (progn ,@body))
                         ,temp)
                  (return ,temp))
                 ((> (get-internal-real-time) ,end-time)
                  (return nil))
                 (t
                  (sleep ,step))))))))

(defmacro wait-until (form &optional (step 0.01))
  "Evaluates BODY each STEP seconds until it evaluates to true, at which point
it returns the value of BODY."
  (with-gensyms (result)
    `(loop for ,result = ,form
           if ,result return ,result
             else do (sleep ,step))))

(defmacro finalized-let* ((&rest bindings) &body body)
  "Like LET*, except each variable binding is of form (var initform . forms)
where FORMS will be evaluated when leaving the LET* by means of UNWIND-PROTECT.
These forms will be evaluated from last binding to first."
  (if bindings
      `(let (,(first (first bindings)))
         (unwind-protect
              (progn (setf ,(first (first bindings))
                           ,(second (first bindings)))
                     (finalized-let* ,(rest bindings) ,@body))
           (when ,(first (first bindings))
             (progn ,@(cddr (first bindings))))))
      `(progn ,@body)))
