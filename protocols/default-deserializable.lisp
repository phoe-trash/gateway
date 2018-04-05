;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/default-deserializable.lisp

(in-package :gateway/protocols)

(define-protocol default-deserializable
    (:description "The DEFAULT-DESERIALIZATION protocol describes a mechanism ~
of automatically setting default deserialization methods for protocol classes. ~
When generic function DESERIALIZE is called, it is able to find the protocol ~
class of the object it is supposed to deserialize, but there are no ~
DESERIALIZE-USING-CLASS methods specializing on protocol classes, but only on ~
concrete implementing classes. This protocol describes a mechanism for making ~
it possible to set and retrieve such classes, which in turn makes it possible ~
to create a generalized DESERIALIZE function, which is also a part of this ~
protocol.
\
This protocol depends on protocol SERIALIZABLE for implementation."
     :tags (:default-deserialization)
     :dependencies (serializable)
     :export t)
  (:function default-deserialization-class
             ((class class)) (implementation-class class))
  "Returns the default deserialization class for the provided class. If no ~
class was set before using (SETF DEFAULT-DESERIALIZATION-CLASS), this function ~
will check the list of all subclasses of the provided class. If there exists ~
exactly one such subclass, it will be returned; otherwise, an error is ~
signaled."
  (:function (setf default-deserialization-class)
             ((new-value class) (class class)) (new-value class))
  "Sets the default deserialization class for the provided class. If NEW-VALUE ~
is NIL, any previously set value is cleared instead, so the function ~
DEFAULT-DESERIALIZATION-CLASS starts exhibiting default behaviour for the ~
provided class."
  (:function deserialize (data) (object serializable))
  "Deserializes the provided data, which can be a Lisp list or a string ~
containing such a list. If supplied a string, this function attempts to safely ~
read the string into its list representation. Then, or when supplied a list, ~
it attempts to find a class matching the first symbol on the list by means of ~
DEFAULT-DESERIALIZATION-CLASS, and then immediately calls ~
DESERIALIZE-USING-CLASS on that class and the list data.")

(defvar *serialization-classes*
  (make-hash-table))

(defmethod default-deserialization-class ((class class))
  (multiple-value-bind (value foundp) (gethash class *serialization-classes*)
    (if foundp
        value
        (let* ((classes (class-direct-subclasses class))) ;; TODO indirect too
          (warn "Default deserialization for class ~A.~%Consider calling ~
\(SETF DEFAULT-DESERIALIZATION-CLASS) for this class." (class-name class))
          (case (length classes)
            (0 (error "Class ~A has no subclasses." (class-name class)))
            (1 (first classes))
            (t (error "Class ~A has more than one subclass."
                      (class-name class))))))))

;; TODO create a protocol implementation file that calls this on all protocols
;; and their implementations
;; TODO create a test that tests all default deserializations
(defmethod (setf default-deserialization-class)
    ((new-value class) (class class))
  (cond ((null new-value)
         (remhash class *serialization-classes*)
         nil)
        (t
         (setf (gethash class *serialization-classes*) new-value))))

(defmethod deserialize (data)
  (check-type data cons)
  (let ((symbol (first data)))
    (check-type symbol symbol)
    (let ((name (symbol-name symbol)))
      (multiple-value-bind (symbol foundp)
          (find-symbol name #.*package*)
        (unless foundp
          (error "No matching symbol has been found for name ~S." name))
        (let ((class (find-class symbol)))
          (unless (not (null class))
            (error "No matching class has been found for symbol ~S." symbol))
          (let ((concrete-class (default-deserialization-class class)))
            (deserialize-using-class concrete-class data)))))))
