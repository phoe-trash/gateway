;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/gateway.protocol.asd

(asdf:defsystem #:gateway.protocol
  :description "Protocols for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:closer-mop
               #:moptilities
               #:protest/protocol
               #:phoe-toolbox
               #:cl-cpus
               #:gateway.utils)
  :serial t
  :components ((:file "package")
               ;; PROTOCOLS - CONFIGURATION
               (:file "configuration/gateway-config")
               ;; PROTOCOLS - MIXINS
               (:file "mixin/serializable")
               (:file "mixin/named")
               (:file "mixin/killable")
               (:file "mixin/addressable")
               (:file "mixin/with-handler")
               ;; PROTOCOLS - MECHANISMS
               (:file "mechanism/config")
               (:file "mechanism/default-deserializable")
               ;; PROTOCOLS - CLASSES
               (:file "class/date")
               ;; (:file "password") ;; unneeded - will be handled by pgsql
               (:file "class/connection")
               (:file "class/acceptor")
               (:file "class/listener")
               (:file "class/kernel")
               ))
