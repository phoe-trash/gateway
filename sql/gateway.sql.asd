;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sql/gateway.sql.asd

(asdf:defsystem #:gateway.sql
  :description "SQL layer for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:cl-yesql
               #:phoe-toolbox
               #:cl+ssl
               #:cl-yesql/postmodern
               #:gateway.utils
               #:gateway.protocols
               #:gateway.config)
  :serial t
  :components ((:file "package")
               (:file "connections")
               (:file "overlord")))
