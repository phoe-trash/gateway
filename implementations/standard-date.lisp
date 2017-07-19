;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; standard-date.lisp

(in-package :gateway/impl)

;; TODO move to constants file, turn into a constant
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *date-granularity-units*
    '(:year :month :day :hour :minute :second :nanosecond)))

(defclass standard-date (date local-time:timestamp) ()
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class DATE.")))

(defmethod serialize ((object standard-date) &key (type :list))
  (let ((data `(:date :string ,(format nil "~A" object))))
    (ecase type
      (:list data)
      (:string (princ-to-string data)))))

(defmethod deserialize-using-class
    ((class (eql (find-class 'standard-date))) data)
  (check-type data cons)
  (assert (= (length data) 3))
  (destructuring-bind (symbol . plist) data
    (check-type symbol symbol)
    (assert (string= symbol :date))
    (let ((string (data-getf plist :string)))
      (check-type string string)
      (change-class (local-time:parse-timestring string)
                    'standard-date))))

(defmethod date-timestamp ((date standard-date))
  (local-time:timestamp-to-unix date))

(defmethod timestamp-date ((timestamp integer))
  ;; TODO add multiple implementation support
  (change-class (local-time:unix-to-timestamp timestamp) 'standard-date))

(defmethod date-nstimestamp ((date standard-date))
  (let ((timestamp (date-timestamp date))
        (nsec (local-time:nsec-of date)))
    (+ (* 1000000000 timestamp) nsec)))

(defmethod nstimestamp-date ((nstimestamp integer))
  (multiple-value-bind (timestamp nsec) (truncate nstimestamp 1000000000)
    (let ((date (timestamp-date timestamp)))
      (setf (local-time:nsec-of date) nsec)
      date)))

(defmethod date= ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :nanosecond))
  (assert (member unit *date-granularity-units*))
  (if (eq unit :nanosecond)
      (local-time:timestamp= date-1 date-2)
      (every #'= (%date-elts date-1 unit) (%date-elts date-2 unit))))

(defun %date-elts (date unit)
  (subseq (nreverse (%date=-decode date)) 4
          (+ 5 (position unit *date-granularity-units*))))

(defun %date=-decode (date)
  (multiple-value-list
   (local-time:decode-timestamp date :timezone local-time:+utc-zone+)))

(defmethod date/= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date<= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date>= ((date-1 standard-date) (date-2 standard-date)
                   &key (unit :nanosecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date> ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :nanosecond))
  (not (date<= date-1 date-2 unit)))

(defmethod date< ((date-1 standard-date) (date-2 standard-date)
                  &key (unit :nanosecond))
  (not (date>= date-1 date-2 unit)))

(defmethod date-min (date &rest other-dates)
  (apply #'local-time:timestamp-minimum date other-dates))

(defmethod date-max (date &rest other-dates)
  (apply #'local-time:timestamp-maximum date other-dates))

(defmethod now ()
  (change-class (local-time:now) 'standard-date))
