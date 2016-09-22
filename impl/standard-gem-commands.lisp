;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem-commands.lisp

(in-package #:gateway)

(defcommand :ping (:n :e :i)
    (crown connection arguments)
  (declare (ignore crown))
  (format t "[~~] Gem: ping-pong: ~S~%" arguments) 
  (send connection (cons :pong arguments)))
