;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; operations/gateway.operations.asd

(asdf:defsystem #:gateway.operations
  :description "Operations doable in Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:gateway.protocols
               #:gateway.impl
               #:gateway.sql)
  :serial t
  :components ((:file "package")
               (:file "install")))
