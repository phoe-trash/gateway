;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; test-messages.lisp

(in-package #:gateway)

;; STANDARD-DATE test
(test test-standard-date
  (with-clean-config
    (flet ((make (day sec nsec) (make-instance 'standard-date :day day :sec sec :nsec nsec)))
      (let* ((date-orig (make 0 0 0)) (date-same (make 0 0 0)) (date-nsec (make 0 0 1))
	     (date-sec (make 0 1 0)) (date-min (make 0 60 0)) (date-hour (make 0 3600 0))
	     (date-day (make 1 0 0)) (date-month (make 31 0 0)) (date-year (make 365 0 0))
	     (vars (list date-orig date-nsec date-sec date-min date-hour date-day date-year)))
	(is (date= date-orig date-same))
	(is (date= date-orig date-same :unit :nanosecond))
	(is (date= date-orig date-nsec :unit :second))
	(is (date= date-orig date-sec :unit :minute))
	(is (date= date-orig date-min :unit :hour))
	(is (date= date-orig date-hour :unit :day))
	(is (date= date-orig date-day :unit :month))
	(is (date= date-orig date-month :unit :year))
	(is (date/= date-orig date-nsec))
	(is (date/= date-orig date-sec))
	(is (date/= date-orig date-min))
	(is (date/= date-orig date-hour))
	(is (date/= date-orig date-day))
	(is (date/= date-orig date-month))
	(is (date/= date-orig date-year))
	(is (date/= date-orig date-sec :unit :nanosecond))
	(is (date/= date-orig date-min :unit :second))
	(is (date/= date-orig date-hour :unit :minute))
	(is (date/= date-orig date-day :unit :hour))
	(is (date/= date-orig date-month :unit :day))
	(is (date/= date-orig date-year :unit :month))
	(is (date< date-orig date-nsec))
	(is (date< date-nsec date-sec))
	(is (date< date-sec date-min))
	(is (date< date-min date-hour))
	(is (date< date-hour date-day))
	(is (date< date-day date-month))
	(is (date< date-month date-year))
	(is (date> date-nsec date-orig))
	(is (date> date-sec date-nsec))
	(is (date> date-min date-sec))
	(is (date> date-hour date-min))
	(is (date> date-day date-hour))
	(is (date> date-month date-day))
	(is (date> date-year date-month))
	(is (eq date-orig (apply #'date-min vars)))
	(is (eq date-year (apply #'date-max vars)))
	(is (date= date-orig (parse (sexp date-orig))))
	(values)))))

;; STANDARD-PASSWORD test
(test test-standard-password
  (with-clean-config
    (let ((wrong-passphrase "Wr0ng-Pas$w0rd"))
      (flet ((check-password (passphrase)
	       (let ((password (make-password passphrase)))
		 (is (password-matches-p password passphrase))
		 (is (not (password-matches-p password wrong-passphrase))))))
	(mapcar #'check-password
		'("" "pass" "password-1" "password-2PassW0RD"
		  "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆąęea
ÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))
	(values)))))

;; STANDARD-CONNECTION test
(test test-standard-connection
  (with-clean-config
    (let* ((date (now))
	   (password (make-password "test-password"))
	   (chat (make-instance 'standard-chat :name "test-chat"))
	   (player (make-instance 'standard-player :name "Player"))
	   (persona (make-instance 'standard-persona :name "Sebastian"
						     :player player)))
      (macrolet ((make (&body body) `(make-instance 'standard-connection ,@body)))
	(with-connections
	    ((connection-listen (make :type :listen))
	     (connection-client (make :type :client))
	     (connection-accept (make :type :accept
				      :socket (socket connection-listen))))
	  (flet ((test-case (data)
		   (send connection-client data)
		   (is (equal (format nil "~S" (sexp (receive connection-accept)))
			      (format nil "~S" (sexp data))))
		   (send connection-accept data)
		   (is (equal (format nil "~S" (sexp (receive connection-client)))
			      (format nil "~S" (sexp data))))))
	    (mapcar #'test-case (list date password chat player persona)))))
      (is (null (maphash (lambda (x y) (list x y)) *connection-cache*))))))

#|
;; STANDARD-MESSAGE test
(test test-standard-message
  (with-clean-config
    (let* ((sender 'sender) (recipient 'recipient) (contents "contents")
	   (date (make-instance 'standard-date :day 1))
	   (message (msg sender recipient contents :date date)))
      (is (equal (sender message) sender))
      (is (equal (recipient message) recipient))
      (is (date= (date message) date))
      (is (string= (contents message) contents))
      (let ((parsed-message (parse (sexp message))))))))
|#

