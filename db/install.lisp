;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/install.lisp

(in-package :gateway/db)

(overlord:set-package-base "" :gateway)

(overlord:import queries-install
  :from #.(asdf:system-relative-pathname :gateway "db/install.sql")
  :as :cl-yesql/postmodern
  :binding :all-as-functions)

(defparameter *install-functions*
  (list #'create-chapter-permission
        #'create-timeline-permission
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
  (mapc #'funcall *uninstall-functions*)
  t)

(defun install ()
  (mapc #'funcall *install-functions*)
  t)

(defun reinstall () (reload) (uninstall) (install))
