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

(defcommand :open-gateway (:n)
    (crown connection arguments) 
  (format t "[~~] Gem: accepting E-connection with comment ~S.~%" arguments)
  (send connection `(ok (open-gateway ,@arguments))) 
  (with-lock-held ((n-lock crown))
    (deletef (n-connections crown) connection))
  (with-lock-held ((e-lock crown))
    (pushnew connection (e-connections crown))))
