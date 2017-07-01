;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; install.lisp

(in-package :gateway/sql)

(define-queries "install/"
  drop-tables
  drop-types
  create-ch-permission
  create-tl-permission
  create-table-timeline
  create-table-chapter
  create-table-player
  create-table-persona
  create-table-post
  create-table-chapter-link
  create-table-chapter-permission
  create-table-timeline-permission
  create-table-global-chapter-permission)

(defun uninstall ()
  (drop-tables)
  (drop-types)
  t)

(defun install (&optional uninstall-p)
  (when uninstall-p
    (uninstall))
  (mapc #'funcall
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
              #'create-table-global-chapter-permission))
  t)
