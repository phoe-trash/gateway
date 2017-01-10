;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-date.lisp

(in-package #:gateway)

(defclass standard-date (date local-time:timestamp) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *date-time-units* '(:year :month :day :hour :minute :second :nanosecond)))

(defmethod sexp ((object standard-date))
  (sexp `(:date :datestring ,(format nil "~A" object))))

(defunsexp date ((datestring :datestring)) ()
  (assert datestring () "Datestring cannot be empty.")
  (parse-date datestring))

(defmethod parse-date ((datestring string))
  (handler-case (change-class (local-time:parse-timestring datestring) 'standard-date)
    (local-time::invalid-timestring ()
      (make-instance 'standard-date))))

(defmethod date= ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :nanosecond))
  (assert (member unit *date-time-units*))
  (if (eq unit :nanosecond)
      (local-time:timestamp= date-1 date-2)
      (every #'= (%date-elts date-1 unit) (%date-elts date-2 unit))))

(defun %date-elts (date unit)
  (subseq (nreverse (%date=-decode date)) 4
          (+ 5 (position unit *date-time-units*))))

(defun %date=-decode (date)
  (multiple-value-list
   (local-time:decode-timestamp date :timezone local-time:+utc-zone+)))

(defmethod date/= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date< ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp< date-1 date-2))

(defmethod date> ((date-1 standard-date) (date-2 standard-date))
  (local-time:timestamp> date-1 date-2))

(defmethod date<= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date>= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date-min (&rest dates)
  (apply #'local-time:timestamp-minimum dates))

(defmethod date-max (&rest dates)
  (apply #'local-time:timestamp-maximum dates))

(defun %now ()
  (change-class (local-time:now) 'standard-date))



(deftest test-standard-date
  (flet ((mkdt (d s ns) (make-instance 'standard-date :day d :sec s :nsec ns)))
    (let* ((d-orig (mkdt 0 0 0)) (d-same (mkdt 0 0 0)) (d-nsec (mkdt 0 0 1))
           (d-sec (mkdt 0 1 0)) (d-min (mkdt 0 60 0)) (d-hour (mkdt 0 3600 0))
           (d-day (mkdt 1 0 0)) (d-month (mkdt 31 0 0)) (d-year (mkdt 365 0 0))
           (vars (list d-orig d-nsec d-sec d-min d-hour d-day d-year)))
      (is (date= d-orig d-same))
      (is (date= d-orig d-same :unit :nanosecond))
      (is (date= d-orig d-nsec :unit :second))
      (is (date= d-orig d-sec :unit :minute))
      (is (date= d-orig d-min :unit :hour))
      (is (date= d-orig d-hour :unit :day))
      (is (date= d-orig d-day :unit :month))
      (is (date= d-orig d-month :unit :year))
      (is (date/= d-orig d-nsec))
      (is (date/= d-orig d-sec))
      (is (date/= d-orig d-min))
      (is (date/= d-orig d-hour))
      (is (date/= d-orig d-day))
      (is (date/= d-orig d-month))
      (is (date/= d-orig d-year))
      (is (date/= d-orig d-sec :unit :nanosecond))
      (is (date/= d-orig d-min :unit :second))
      (is (date/= d-orig d-hour :unit :minute))
      (is (date/= d-orig d-day :unit :hour))
      (is (date/= d-orig d-month :unit :day))
      (is (date/= d-orig d-year :unit :month))
      (is (date< d-orig d-nsec))
      (is (date< d-nsec d-sec))
      (is (date< d-sec d-min))
      (is (date< d-min d-hour))
      (is (date< d-hour d-day))
      (is (date< d-day d-month))
      (is (date< d-month d-year))
      (is (date> d-nsec d-orig))
      (is (date> d-sec d-nsec))
      (is (date> d-min d-sec))
      (is (date> d-hour d-min))
      (is (date> d-day d-hour))
      (is (date> d-month d-day))
      (is (date> d-year d-month))
      (is (eq d-orig (apply #'date-min vars)))
      (is (eq d-year (apply #'date-max vars)))
      (is (date= d-orig (unsexp (sexp d-orig)))))))
