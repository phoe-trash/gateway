;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; install.lisp

(in-package :gateway/install)


;; TODO rewrite with CL-SQL
(defun reload ()
  (define-queries "install/sql/"
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
    create-table-global-timeline-permission))

(defun uninstall ()
  (drop-tables)
  (drop-types)
  t)

(defun install ()
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
