;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; ping.lisp

(in-package #:gateway)

#|
Command PING

This command is a basic ping.

Arguments:
* DATA: arbitrary SEXPABLE data to be transferred back to the pinger.
|#

(defcommand ping (owner connection)
            (:data)
  (note "[C] Got pinged with data ~S.~%" data)
  (data-send connection (list :pong :data data)))

(deftest test-command-ping
  (with-crown-and-connections crown (connection) ()
    (data-send connection '(:ping :data (1 2 3 4 5)))
    (is (wait () (data-equal (data-receive connection)
                             '(:pong :data (1 2 3 4 5)))))))
