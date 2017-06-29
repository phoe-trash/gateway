;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; test-messages.lisp

(in-package #:gateway)

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

