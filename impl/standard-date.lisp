;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-date.lisp

(in-package #:gateway)

(defclass standard-date (date local-time:timestamp) ())

(defmethod sexp ((object standard-date))
  (sexp `(#:date #:date ,(format nil "~A" object))))

(defmethod parse-date ((datestring string))
  (handler-case (change-class (local-time:parse-timestring datestring) 'standard-date)
    (local-time::invalid-timestring ()
      (make-instance 'standard-date))))

(defmethod date= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (flet ((%date= (date-1 date-2 unit)
	   (check-type unit (member :year :month :day :hour :minute :second :nanosecond))
	   (let ((units '(:year :month :day :hour :minute :second :nanosecond))) 
	     (labels ((v (date) (multiple-value-list
                                 (local-time:decode-timestamp date :timezone local-time:+utc-zone+)))
		      (d (date) (subseq (nreverse (v date)) 4 (+ 5 (position unit units)))))
	       (every #'= (d date-1) (d date-2))))))
    (if (eq unit :nanosecond)
	(local-time:timestamp= date-1 date-2)
	(%date= date-1 date-2 unit))))

(defmethod date/= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date< ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp< date-1 date-2))

(defmethod date> ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp> date-1 date-2))

(defmethod date<= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date>= ((date-1 standard-date) (date-2 standard-date) &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date-min (&rest dates)
  (apply #'local-time:timestamp-minimum dates))

(defmethod date-max (&rest dates)
  (apply #'local-time:timestamp-maximum dates))

(defmethod now ()
  (change-class (local-time:now) 'standard-date))
