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

(defcommand ping (owner connection) (:data)
  (note "[C] Got pinged with data ~S.~%" data)
  (data-send connection (list :pong :data data)))

(deftest test-command-ping
  (let ((data '(1 2 3 4 5)))
    (with-crown-and-connections crown (connection) ()
      (%test connection
             `(:ping :data ,data)
             `(:pong :data ,data)))))
