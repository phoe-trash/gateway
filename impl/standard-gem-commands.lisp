;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem-commands.lisp

(in-package #:gateway)

(defcommand :ping (:e :i)
    (crown connection &rest arguments)
  (declare (ignore crown))
  (format t "[~~] Gem: ping-pong: ~S~%" arguments) 
  (send connection (cons :pong arguments)))

;; (defcommand :open-gateway (:n)
;;     (crown connection &rest arguments)
;;   (if arguments
;;       (format t "[~~] Gem: accepting E-connection ~S.~%" arguments)
;;       (format t "[~~] Gem: accepting E-connection.~%"))
;;   (send connection `(ok (open-gateway ,@arguments))))

(defcommand :login (:n)
    (crown connection username password)
  (declare (ignore password))
  (cond ((auth connection)
         (format t "[!] Gem: user ~S already logged in.~%" username)
         (send connection `(error :already-logged-in ,username)))
        ((lookup (library crown) `(auth ,username))
         (format t "[!] Gem: username ~S already taken.~%" username)
         (send connection `(error :username-taken ,username)))
        (t
         (format t "[~~] Gem: logging user ~S in.~%" username)
         (setf (lookup (library crown) `(auth ,username)) connection
               (auth connection) `(user ,username))
         (with-lock-held ((n-lock crown))
           (deletef (n-connections crown) connection))
         (with-lock-held ((e-lock crown))
           (pushnew connection (e-connections crown)))
         (send connection `(ok (login ,username))))))

(defcommand :logout (:n :e)
    (crown connection)
  (let ((username (second (auth connection))))
    (cond ((null (auth connection))
           (format t "[!] Gem: LOGOUT: user not logged in.~%")
           (send connection `(error :not-logged-in)))
          (t
           (format t "[~~] Gem: logging user ~S out.~%" username)
           (setf (lookup (library crown) `(auth ,username)) nil
                 (auth connection) nil)
           (with-lock-held ((e-lock crown))
             (deletef (e-connections crown) connection))
           (with-lock-held ((n-lock crown))
             (pushnew connection (n-connections crown)))
           (send connection `(ok (logout)))))))

(defcommand :emit (:e)
    (crown connection message)
  (cond ((null (auth connection)) 
         (format t "[!] Gem: EMIT: user not logged in.~%")
         (send connection `(error :not-logged-in)))
        (t
         (let ((username (second (auth connection))))
           (format t "[!] Gem: Emit from ~S: ~S~%" username message)
           (send connection `(ok (emit ,message)))
           (mapcar (lambda (x) (send x `(emit ,username ,message)))
                   (e-connections crown))))))

