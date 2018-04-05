;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/install.lisp

(in-package :gateway/sql)

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

(defun install () (mapc #'funcall *install-functions*) t)

(defun uninstall () (mapc #'funcall *uninstall-functions*) t)

(defun reinstall ()
  (import-all)
  (overlord:build)
  (uninstall)
  (install))
