;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-library.lisp

(in-package #:gateway)

(defclass standard-library (library)
  ((%hash-table :accessor hash-table-of
                :initform (make-hash-table :test #'equal))
   (%lock :accessor lock
          :initform (make-lock))))

(defmethod lookup ((library standard-library) key)
  (with-lock-held ((lock library))
    (gethash key (hash-table-of library))))

(defmethod (setf lookup) (new-value (library standard-library) key)
  (with-lock-held ((lock library))
    (setf (gethash key (hash-table-of library)) new-value)))
