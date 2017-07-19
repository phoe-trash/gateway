;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; immutable-class.lisp

(in-package :gateway/protocols)

(define-protocol immutable-class
    (:description "The IMMUTABLE-CLASS protocol describes a class metaobject. ~
Conceptually, this metaclass describes classes that are immutable, meaning, ~
whose contents should not change. Instances of classes which have this ~
metaclass are prohibited from modifying their slot values by any means, ~
including writer functions and SETF SLOT-VALUE."
     :tags (immutable immutable-class) :export t)
  (:class immutable-class (standard-class) ())
  "A protocol metaclass for immutable classes.")

(defclass standard-immutable-class (immutable-class) ())

(defmethod validate-superclass ((class immutable-class)
                                (superclass standard-class))
  t)

(defmethod (setf slot-value-using-class)
    (new-value (class immutable-class) object slot)
  (declare (ignore new-value))
  (error "Attempted to modify slot ~A of ~S, which is an instance of an ~
immutable class ~A." slot object class))
