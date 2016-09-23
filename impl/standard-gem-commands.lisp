;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem-commands.lisp

(in-package #:gateway)

(defcommand :ping (:n :e :i)
    (crown connection &rest arguments)
  (declare (ignore crown))
  (format t "[~~] Gem: ping-pong: ~S~%" arguments) 
  (send connection (cons :pong arguments)))

(defcommand :open-gateway (:n)
    (crown connection &rest arguments)
  (if arguments
      (format t "[~~] Gem: accepting E-connection ~S.~%" arguments)
      (format t "[~~] Gem: accepting E-connection.~%"))
  (send connection `(ok (open-gateway ,@arguments))) 
  (with-lock-held ((n-lock crown))
    (deletef (n-connections crown) connection))
  (with-lock-held ((e-lock crown))
    (pushnew connection (e-connections crown))))

(defcommand :login (:n :e)
    (crown connection username password)
  (declare (ignore password))
  (if (auth connection)
      (send connection `(error :already-logged-in ,username)) 
      (let* ((library (library crown))
             (lookup (lookup library `(auth ,connection))))
        (if lookup
            (send connection `(error :username-taken ,username))
            (progn
              (setf (lookup library `(auth ,connection)) `(user ,username)
                    (auth connection) `(user ,username))
              (send connection `(ok (login ,username))))))))
