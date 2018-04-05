;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; config/gateway.config.asd

(asdf:defsystem #:gateway.config
  :description "Configuration for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:gateway.utils
               #:gateway.protocols)
  :serial t
  :components ((:file "package")
               (:file "config")))
