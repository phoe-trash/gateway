;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; install.lisp

(in-package :gateway/install)

(overlord:set-package-base "" :gateway)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reload ()
    (overlord:import my-queries
      :from #.(asdf:system-relative-pathname :gateway "install/install.sql")
      :as :cl-yesql/postmodern
      :binding :all-as-functions))
  (reload))

(defparameter *install-functions*
  (list #'create-ch-permission
        #'create-tl-permission
        #'create-table-timeline
        #'create-table-chapter
        #'create-table-player
        #'create-table-persona
        #'create-table-post
        #'create-table-chapter-link
        #'create-table-chapter-permission
        #'create-table-timeline-permission
        #'create-table-global-timeline-permission))

(defparameter *uninstall-functions*
  (list #'drop-tables
        #'drop-types))

(defun uninstall ()
  (let* ((postmodern:*database* *db-connection*)
         (db postmodern:*database*))
    (assert (and db (postmodern:connected-p db)) ()
            "The database is not connected.")
    (mapc #'funcall *uninstall-functions*)
    t))

(defun install ()
  (let* ((postmodern:*database* *db-connection*)
         (db postmodern:*database*))
    (assert (and db (postmodern:connected-p db)) ()
            "The database is not connected.")
    (mapc #'funcall *install-functions*)
    t))
