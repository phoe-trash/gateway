;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; serializable.lisp

(in-package :gateway/protocols)

(define-protocol serializable
    (:description "The SERIALIZABLE protocol describes objects which are ~
convertible between their internal Lisp representation and a readable text ~
representation in form of a readable S-expression. Such S-expressions:
* must be proper lists and must not contain improper lists,
* must not contain any reader macros other than for characters #\\( #\\) #\\\",
* can only consist of proper lists, numbers, symbols and strings."
     :tags (:serializable) :export t)
  (:class serializable () ())
  "A serializable object, participating in the SERIALIZABLE protocol."
  (:function serialize ((object serializable) &key type) serialized-data)
  "Serializes the target object. If the :TYPE key parameter is :LIST, the ~
object is serialized into its S-expression representation. If it is :STRING, ~
that representation is additionally printed to a readable string, which is ~
then returned. The default is :LIST."
  (:function deserialize-using-class
             ((class (or class symbol)) data) (object serializable))
  "Deserializes the provided data, which is a proper list, as an object of the ~
provided class. If CLASS is a symbol, this function calls FIND-CLASS to find ~
the concrete class object.")
