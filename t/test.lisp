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
    (let* ((password-1 (make-password "password-1"))
	   (password-2 (make-password "password-2PassW0RD"))
	   (password-3 (make-password "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))
	   (password-4 (make-password "")))
      (is (password-matches-p password-1 "password-1"))
      (is (password-matches-p password-2 "password-2PassW0RD"))
      (is (password-matches-p password-3 "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))
      (is (password-matches-p password-4 ""))
      (is (not (password-matches-p password-1 "Wr0ng-Pas$w0rd")))
      (is (not (password-matches-p password-2 "Wr0ng-Pas$w0rd")))
      (is (not (password-matches-p password-3 "Wr0ng-Pas$w0rd")))
      (is (not (password-matches-p password-4 "Wr0ng-Pas$w0rd"))))))

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
