;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; date.lisp

(in-package #:gateway)

#|
Protocol class DATE

Must be SEXPABLE and IMMUTABLE.

The :UNIT argument must accept arguments
:YEAR, :MONTH, :DAY, :HOUR, :MINUTE, :SECOND, :NANOSECOND.
|#
(defprotocol date
    (date () ())
  (defgeneric parse-date (string))
  (defgeneric date= (date-1 date-2 &key unit))
  (defgeneric date/= (date-1 date-2 &key unit))
  (defgeneric date< (date-1 date-2))
  (defgeneric date<= (date-1 date-2 &key unit))
  (defgeneric date> (date-1 date-2))
  (defgeneric date>= (date-1 date-2 &key unit))
  (defgeneric date-min (&rest dates))
  (defgeneric date-max (&rest dates))
  (defun now () (%now)))
