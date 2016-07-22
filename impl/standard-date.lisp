;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-date.lisp

(in-package #:gateway)

(defclass standard-date (date local-time:timestamp) ())

(defmethod sexp ((object standard-date))
  `(:date ,(format nil "~A" object)))

(defmethod parse-date ((datestring string))
  (change-class (local-time:parse-timestring datestring) 'standard-date))

(defmethod date= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond)) 
  (if (eq unit :nanosecond)
      (local-time:timestamp= date-1 date-2)
      (%fuzzy-date= date-1 date-2 unit)))

(defun %fuzzy-date= (date-1 date-2 unit) 
  (let ((units '(:year :month :day :hour :minute :second :nanosecond)))
    (check-type unit (member :year :month :day :hour :minute :second :nanosecond))
    (labels ((get-vals (date) (multiple-value-list (local-time:decode-timestamp date)))
	     (get-date (date)
	       (subseq (nreverse (get-vals date)) 4 (+ 5 (position unit units)))))
      (every #'= (get-date date-1) (get-date date-2)))))

(defmethod date/= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date< ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp< date-1 date-2))

(defmethod date<= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date> ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp> date-1 date-2))

(defmethod date>= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date-min ((date-1 standard-date) &rest dates)
  (apply #'local-time:timestamp-minimum date-1 dates))

(defmethod date-max ((date-1 standard-date) &rest dates)
  (apply #'local-time:timestamp-maximum date-1 dates))
