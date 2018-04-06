;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/install.lisp

(in-package :gateway/operations)

(defparameter *install-functions*
  (list #'gateway/sql:create-chapter-permission
        #'gateway/sql:create-timeline-permission
        #'gateway/sql:create-table-timeline
        #'gateway/sql:create-table-chapter
        #'gateway/sql:create-table-player
        #'gateway/sql:create-table-persona
        #'gateway/sql:create-table-post
        #'gateway/sql:create-table-chapter-link
        #'gateway/sql:create-table-chapter-permission
        #'gateway/sql:create-table-timeline-permission
        #'gateway/sql:create-table-global-timeline-permission))

(defparameter *uninstall-functions*
  (list #'gateway/sql:drop-tables
        #'gateway/sql:drop-types))

(defun install ()
  "Installs a fresh Gateway schema into the currently selected database."
  (mapc #'funcall *install-functions*)
  t)

(defun uninstall ()
  "Drops the Gateway schema from the currently selected database."
  (mapc #'funcall *uninstall-functions*)
  t)

(defun reinstall (&optional rebuild-sql-p (actually-reinstall-p t))
  "Deletes and recreates the Gateway database schema. If REBUILD-SQL-P is
provided, this function also recompiles all SQL commands. If
ACTUALLY-REINSTALL-P is false, the reinstallation does not occur."
  (when rebuild-sql-p (gateway/sql::import-all))
  (when actually-reinstall-p (uninstall) (install)))
