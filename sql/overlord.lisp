;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/overlord.lisp

(in-package :gateway/sql)

(overlord:set-package-base "yesql/" :gateway.sql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '((install . "install.sql")
      (player . "player.sql"))))

(defun import-all ()
  (let ((*package* (find-package :gateway/sql)))
    (eval `(prog1 t ,@(loop for (name . sql) in *sql-imports*
                            with *package* = (find-package :gateway/sql)
                            collect `(overlord:import ,name
                                       :from ,sql
                                       :as :cl-yesql/postmodern
                                       :binding :all-as-functions
                                       :export-bindings-p t))))))

(import-all)
