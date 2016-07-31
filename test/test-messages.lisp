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
	(is (date= date-orig (parse (sexp date-orig)))))))
  (values))

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
ÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś")))))
  (values))

;; STANDARD-CONNECTION test
(test test-standard-connection
  (with-clean-config
    (labels ((check-conns () (is (null (maphash #'list *connection-cache*)))))
      (kill (make-instance 'standard-connection :type :listen))
      (check-conns)
      (let* ((date (now))
	     (password (make-password "password"))
	     (chat (make-instance 'standard-chat :name "chat"))
	     (player (make-instance 'standard-player :name "player"))
	     (persona (make-instance 'standard-persona :name "persona" :player player))
	     (message (msg persona chat "message" :date date)))
	(macrolet ((make (&body body) `(make-instance 'standard-connection ,@body)))
	  (with-connections
	      ((listen (make :type :listen))
	       (client (make :type :client))
	       (accept (make :type :accept :socket (socket listen))))
	    (labels ((form (data) (format nil "~S" (sexp data)))
		     (test (x y data)
		       (send x data)
		       (is (readyp y)) 
		       (is (equal (form (receive y)) (form data))))
		     (test-case (data)
		       (test client accept data)
		       (test accept client data)))
	      (mapcar #'test-case (list date password chat player persona message))))))
      (check-conns)))
  (values))

;; STANDARD-PLAYER, STANDARD-MESSAGE and STANDARD-PERSONA test
(test test-standard-player-persona-message
  (with-clean-config
    (macrolet ((make (&body body) `(make-instance 'standard-connection ,@body))
	       (mkpl (&body body) `(make-instance 'standard-player ,@body))
	       (mkpe (&body body) `(make-instance 'standard-persona ,@body)))
      ;; TODO: rewrite connections using server loop
      (with-connections
	  ((listen (make :type :listen))
	   (client (make :type :client))
	   (accept (make :type :accept :socket (socket listen))))
	(let* ((name "test-name")
	       (email "test@email.com")
	       (password (make-password "test-password"))
	       (player (mkpl :name name :email email
			     :password password :connection accept)))
	  (is (eq (name player) name))
	  (is (eq (email player) email))
	  (is (eq (password player) password))
	  (is (eq (connection player) accept))
	  (is (eq (personas player) nil))
	  (is (eq (find-player name) player))
	  (signals simple-error
	    (make-instance 'standard-player :name name))
	  (let ((persona-1 (mkpe :name "test-persona-1" :player player))
		(persona-2 (mkpe :name "test-persona-2" :player player)))
	    (is (find persona-1 (personas player)))
	    (is (find persona-2 (personas player)))
	    (let* ((player-2 (mkpl :name "test-name-2" :email "test2@email.com"
				   :password password :connection client))
		   (persona-3 (mkpe :name "test-persona-3" :player player-2))
		   (message (msg persona-3 persona-1 "test-message")))
	      (send-message message player) 
	      (let ((message-2 (receive (connection player-2))))
		(is (message= message message-2)))))))))
  (values))

;; STANDARD-CHAT test
(test test-standard-chat
  (with-clean-config
    (macrolet ((make (&body body) `(make-instance 'standard-connection ,@body))
	       (mkpl (&body body) `(make-instance 'standard-player ,@body))
	       (mkpe (&body body) `(make-instance 'standard-persona ,@body))
	       (mkch (&body body) `(make-instance 'standard-chat ,@body))
	       (mkpw (&body body) `(make-password ,@body)))
      ;; TODO: rewrite connections using server loop
      (with-connections
	  ((listen (make :type :listen))
	   (client-1 (make :type :client))
	   (accept-1 (make :type :accept :socket (socket listen))) 
	   (client-2 (make :type :client))
	   (accept-2 (make :type :accept :socket (socket listen))))
	(let* ((pass-1 (mkpw "password-1"))
	       (pass-2 (mkpw "password-2"))
	       (player-1 (mkpl :name "name-1" :email "email-1"
			       :password pass-1 :connection accept-1))
	       (player-2 (mkpl :name "name-2" :email "email-2"
			       :password pass-2 :connection accept-2))
	       (persona-1 (mkpe :name "test-persona-1" :player player-1))
	       (persona-2 (mkpe :name "test-persona-2" :player player-1))
	       (persona-3 (mkpe :name "test-persona-3" :player player-2))
	       (chat (mkch :name "chat-1"))
	       (message-1 (msg persona-1 persona-3 "contents-1"))
	       (message-2 (msg persona-3 persona-2 "contents-2")))
	  (is (string= (name chat) "chat-1"))
	  (is (eq (messages chat) nil))
	  (is (eq (personas chat) nil))
	  (add-persona persona-1 chat)
	  (is (find persona-1 (personas chat)))
	  (add-persona persona-2 chat)
	  (add-persona persona-3 chat)
	  (is (find persona-2 (personas chat)))
	  (is (find persona-3 (personas chat)))
	  (flet ((test-message (message) 
		   (send-message message chat)
		   (is (message= message (receive client-1)))
		   (is (message= message (receive client-2)))
		   (is (not (readyp client-1)))
		   (is (not (readyp client-2)))))
	    (test-message message-1)
	    (test-message message-2))))))
  (values))



