;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; date.lisp

(in-package :gateway/protocols)

;; TODO: check the "generic function clobbers an earlier FTYPE proclamation"
;; style-warnings
(define-protocol date
    (:description "The DATE protocol describes a timestamp object, ~
representing a point in time. These objects are immutable, have millisecond ~
precision and can be compared to other timestamp objects, converted to string ~
and from string representations, precisely Unix timestamps.

The time units, available in comparison functions, are :YEAR :MONTH :DAY :HOUR ~
:MINUTE :SECOND :MILLISECOND. If the key argument UNIT is supplied with one of ~
these values, the comparison takes into account that unit's granularity. The ~
default unit is :MILLISECOND.

Examples:
* 29th July 2017 and 30th July 2017 will not be DATE= under :UNIT :DAY, but ~
will be equal under :UNIT :MONTH.
* 31st July 2017 and 1st August 2017 will not be DATE= under :UNIT :DAY or
:MONTH, but will be equal under :UNIT :YEAR."
     :tags (date) :export t)
  (:class date (serializable) ())
  "A timestamp object, representing a point in time."
  (:function date-timestamp ((date date)) (timestamp integer))
  "Converts a date object to a Unix timestamp."
  (:function timestamp-date ((timestamp integerp)) (date date))
  "Converts a Unix timestamp to a date object."
  (:function date-ustimestamp ((date date)) (nstimestamp integer))
  "Converts a date object to a Unix timestamp with microsecond precision."
  (:function ustimestamp-date ((nstimestamp integer)) (date date))
  "Converts a Unix timestamp with microsecond precision to a date object."
  (:function date= ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the two dates are equal under the provided granularity ~
unit."
  (:function date/= ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the two dates are not equal under the provided granularity ~
unit."
  (:function date> ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the first date is greater than the other under the ~
provided granularity unit."
  (:function date>= ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the first date is not less than the other under the ~
provided granularity unit."
  (:function date< ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the first date is less than the other under the provided ~
granularity unit."
  (:function date<= ((date-1 date) (date-2 date) &key) :generalized-boolean)
  "Returns true iff the first date is not greater than the other under the ~
provided granularity unit."
  (:function date-min ((date date) &rest other-dates) (date-min date))
  "Returns the oldest date from all provided dates."
  (:function date-max ((date date) &rest other-dates) (date-max date))
  "Returns the newest date from all provided dates."
  (:function now () (now date))
  "Returns the date object that corresponds to the current time, relative to ~
the call of this function.")
