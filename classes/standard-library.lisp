;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-library.lisp

(in-package #:gateway)

(defclass standard-library (library)
  ((%hash-table :accessor hash-table-of)
   (%lock :accessor lock
          :initform (make-lock "STANDARD-LIBRARY"))
   (%type :accessor %type
          :initform nil)))

(defconstructor (standard-library type (test #'eql))
  (setf (hash-table-of standard-library) (make-hash-table :test test))
  (when (eq type :crown)
    (labels ((library-hash-table (keyword hash-table-test &rest rest)
               (setf (lookup keyword standard-library)
                     (make-instance 'standard-library :test hash-table-test))
               (unless (null rest) (apply #'library-hash-table rest))))
      (library-hash-table :players #'equal)))
  (setf (%type standard-library) type))

(defmethod lookup (key (library standard-library))
  (with-lock-held ((lock library))
    (multiple-value-bind (value foundp) (gethash key (hash-table-of library))
      (when (and (eq (%type library) :crown) (not foundp))
        (error "Library element not found."))
      (values value foundp))))

(defmethod (setf lookup) (new-value key (library standard-library))
  (with-lock-held ((lock library))
    (when (eq (%type library) :crown)
      (error "(SETF LIBRARY) called on a crown library."))
    (setf (gethash key (hash-table-of library)) new-value)))



(deftest test-standard-library
  ;; (let* ((library (make-instance 'standard-library))
  ;;        (maximum #.(expt 2 15))
  ;;        (number-1 (random maximum))
  ;;        (number-2 (random maximum))
  ;;        (number-3 (random maximum)))
  ;;   (setf (lookup library '(a a 1)) number-1)
  ;;   (is (= (lookup library '(a a 1)) number-1))
  ;;   (setf (lookup library '(a a 1)) number-2)
  ;;   (is (= (lookup library '(a a 1)) number-2))
  ;;   (setf (lookup library '(a b 1)) number-3)
  ;;   (is (= (lookup library '(a b 1)) number-3)))
  )
