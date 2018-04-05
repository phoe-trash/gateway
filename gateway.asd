;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; gateway.asd

(asdf:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.utils
               #:gateway.protocols
               #:gateway.framework
               #:gateway.config
               #:gateway.impl
               #:gateway.sql
               #:gateway.operations)
  :serial t
  :components ((:file "package")))
