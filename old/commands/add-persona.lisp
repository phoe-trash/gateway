;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; add-persona.lisp

(in-package #:gateway)

#|
Command ADD-PERSONA

This command allows the user to add a persona to their account.

Arguments:
* NAME: the name for the newly created persona.
|#

(defcommand add-persona (owner connection) ((:name string))
  (check-type owner crown)
  (let ((player (auth connection)))
    (unless player (error 'not-logged-in :connection connection))
    (unless (typep player 'player)
      (error 'not-authorized :command :add-persona :connection connection))
    (let ((persona (lookup name (library owner :personas))))
      (when persona (error 'name-taken :name name)))
    (let ((persona (%make-persona name player)))
      (with-lock-held ((lock player))
        (push persona (personas player)))
      (setf (lookup name (library owner :personas)) persona)
      (data-send connection '(:ok :add-persona)))))

(deftest test-command-add-persona
  (let* ((username "test-username")
         (password "test-password")
         (email "test-email@email.com")
         (name "test-persona")
         (player (%make-player username password email))
         (login-ok '(:ok :login))
         (add-persona-ok '(:ok :add-persona))
         (name-taken `(:error :type :name-taken :name ,name)))
    (with-crown-and-connections crown (connection) ()
      (setf (lookup username (library crown :players)) player)
      (%test connection
             `(login :username ,username :password ,password)
             login-ok
             `(add-persona :name ,name)
             add-persona-ok)
      (is (find name (personas player) :key #'name :test #'string=))
      (is (string= name (name (lookup name (library crown :personas)))))
      (%test connection
             `(add-persona :name ,name)
             name-taken))))
