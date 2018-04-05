;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/overlord.lisp

(in-package :gateway/sql)

(overlord:set-package-base "sql/yesql/" :gateway)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '((install . "install.sql")
      (player . "player.sql"))))

(defmacro import-all ()
  `(progn ,@(loop for (name . sql) in *sql-imports*
                  collect `(overlord:import ,name
                             :from ,sql
                             :as :cl-yesql/postmodern
                             :binding :all-as-functions
                             :export-bindings-p t))))

(import-all)
