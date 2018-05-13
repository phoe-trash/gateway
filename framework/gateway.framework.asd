;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; framework/gateway.framework.asd

(asdf:defsystem #:gateway.framework
  :description "Framework for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:phoe-toolbox
               #:gateway.protocol)
  :serial t
  :components ((:file "package")
               (:file "with-restartability")))
