;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; gateway.asd

(asdf:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:asdf
               #:overlord
               #:cl-yesql
               #:cl-yesql/postmodern
               #:protest
               #:cl-cpus
               #:safe-read
               #:postmodern
               #:1am
               #:trivial-arguments
               #:trivia
               #:cl-ppcre
               #:ironclad
               #:usocket
               #:bordeaux-threads
               #:lparallel
               )
  :serial t
  :components (;; PACKAGE
               (:file "package")
               ;; UTILS
               (:file "utils/functions")
               (:file "utils/macros")
               (:file "utils/prinr-to-string")
               (:file "utils/verify-arguments")
               ;; VARIABLES
               (:file "variables/constants")
               ;; INSTALL
               (:file "install/install")
               ;; PROTOCOLS - VARIA
               (:file "protocols/config")
               ;; PROTOCOLS - MIXINS
               (:file "protocols/serializable")
               (:file "protocols/default-deserializable")
               (:file "protocols/named")
               (:file "protocols/killable")
               ;; PROTOCOLS - CLASSES
               (:file "protocols/date")
               (:file "protocols/password")
               (:file "protocols/connection")
               (:file "protocols/acceptor")
               (:file "protocols/listener")
               (:file "protocols/kernel")
               ;; FRAMEWORK
               (:file "framework/with-restartability")
               ;; CONFIG
               (:file "config/config")
               ;; CLASSES
               (:file "classes/standard-date")
               (:file "classes/standard-password")
               (:file "classes/standard-socket")
               (:file "classes/standard-connection")
               (:file "classes/standard-acceptor")
               (:file "classes/standard-listener")
               (:file "classes/standard-kernel")
               ))
