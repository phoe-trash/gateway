;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-gem-commands.lisp

(in-package #:gateway)

(defcommand :ping (:e :i)
    (crown connection &rest arguments)
  (declare (ignore crown))
  (format t "[~~] Gem: ping-pong: ~S~%" arguments) 
  (send connection (cons :pong arguments)))

(defcommand :login (:n)
    (crown connection username password)
  (declare (ignore password))
  (cond ((lookup (library crown) `(auth ,username))
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

(defcommand :logout (:e)
    (crown connection)
  (let ((username (second (auth connection))))
    (format t "[~~] Gem: logging user ~S out.~%" username)
    (setf (lookup (library crown) `(auth ,username)) nil
	  (auth connection) nil)
    (with-lock-held ((e-lock crown))
      (deletef (e-connections crown) connection))
    (send connection `(ok (logout)))
    (kill connection)))

(defcommand :emit (:e)
    (crown connection message)
  (let ((username (second (auth connection))))
    (format t "[!] Gem: Emit from ~S: ~S~%" username message)
    (send connection `(ok (emit ,message)))
    (mapc (lambda (x) (send x `(emit ,username ,message)))
	  (e-connections crown))))

(defcommand :online (:e)
    (crown connection)
  (let ((names (with-lock-held ((e-lock crown))
                 (mapcar (compose #'second #'auth)
                         (e-connections crown)))))
    (format t "[!] Gem: Whois from ~S.~%" connection)
    (send connection `(ok (whois)))
    (send connection `(whois ,@names))))
