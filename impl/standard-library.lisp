;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-library.lisp

(in-package #:gateway)

(defclass standard-library (library)
  ((%hash-table :accessor hash-table-of
                :initform (make-hash-table :test #'equal))
   (%lock :accessor lock
          :initform (make-lock "STANDARD-LIBRARY"))))

(defmethod lookup ((library standard-library) key)
  (with-lock-held ((lock library))
    (gethash key (hash-table-of library))))

(defmethod (setf lookup) (new-value (library standard-library) key)
  (with-lock-held ((lock library))
    (setf (gethash key (hash-table-of library)) new-value)))

(deftest test-standard-library
  (let* ((library (make-instance 'standard-library))
         (maximum #.(expt 2 15))
         (number-1 (random maximum))
         (number-2 (random maximum))
         (number-3 (random maximum)))
    (setf (lookup library '(a a 1)) number-1)
    (is (= (lookup library '(a a 1)) number-1))
    (setf (lookup library '(a a 1)) number-2)
    (is (= (lookup library '(a a 1)) number-2))
    (setf (lookup library '(a b 1)) number-3)
    (is (= (lookup library '(a b 1)) number-3))))
