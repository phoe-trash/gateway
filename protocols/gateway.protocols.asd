;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/gateway.protocols.asd

(asdf:defsystem #:gateway.protocols
  :description "Protocols for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:closer-mop
               #:protest
               #:phoe-toolbox
               #:gateway.utils)
  :serial t
  :components ((:file "package")
               ;; PROTOCOLS - VARIA
               (:file "config")
               ;; PROTOCOLS - MIXINS
               (:file "serializable")
               (:file "default-deserializable")
               (:file "named")
               (:file "killable")
               ;; PROTOCOLS - CLASSES
               (:file "date")
               (:file "password")
               (:file "connection")
               (:file "acceptor")
               (:file "listener")
               (:file "kernel")
               ))
