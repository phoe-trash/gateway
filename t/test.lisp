;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; test.lisp

(in-package #:gateway)

(defmacro with-clean-cache (&body body)
  `(let (*persona-cache* *player-cache* *socket-cache*)
     ,@body))

(test %%password-test
  (with-clean-cache
    (let ((wrong-passphrase "Wr0ng-Pas$w0rd"))
      (flet ((check-password (passphrase)
	       (let ((password (make-password passphrase)))
		 (is (password-matches-p password passphrase))
		 (is (not (password-matches-p password wrong-passphrase))))))
	(mapcar #'check-password
		'("" "pass" "password-1" "password-2PassW0RD"
		  "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆ
ÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))))))

#|
(test %%general-test
  (with-clean-cache
    (let* ((player-1 (make-instance 'player :username "player-1" :id 1))
	   (player-2 (make-instance 'player :username "player-1" :id 2))
	   (persona-1 (make-instance 'persona :name "persona-1" :id 3))
	   (persona-2 (make-instance 'persona :name "persona-2" :id 4))
	   (chat (make-instance 'chat :name "test chat" :id 5))
	   (message-1 (msg persona-1 persona-2 "test message 1"))
	   (message-2 (msg persona-2 persona-1 "test message 2")))
      )))
|#
