;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; impl/gateway.impl.asd

(asdf:defsystem #:gateway.impl
  :description "Implementation of Gateway protocols"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:named-readtables
               #:alexandria
               #:closer-mop
               #:cl-cpus
               #:1am
               #:safe-read
               #:usocket
               #:bordeaux-threads
               #:lparallel
               #:verbose
               #:protest
               #:phoe-toolbox
               #:gateway.utils
               #:gateway.protocol
               ;; #:gateway.framework
               )
  :serial t
  :components ((:file "package")
               (:file "standard-date")
               (:file "standard-password")
               (:file "standard-socket")
               (:file "standard-connection")
               (:file "standard-acceptor")
               (:file "standard-listener")
               (:file "standard-kernel")))
