;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/player.lisp

(in-package :gateway/db)

(overlord:import queries-player
  :from #.(asdf:system-relative-pathname :gateway "db/player.sql")
  :as :cl-yesql/postmodern
  :binding :all-as-functions)

(defun reload ()
  (overlord:build 'queries-install 'queries-player))
