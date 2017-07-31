;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; gateway.asd

(asdf:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "GPL3"
  :depends-on (#:alexandria
               #:overlord
               #:cl-yesql
               #:protest
               #:postmodern
               #:1am
               #:trivial-arguments
               #:trivia
               #:cl-ppcre
               #:asdf)
  :serial t
  :components (;; PACKAGE
               (:file "package")
               ;; UTILS
               (:file "utils/functions")
               (:file "utils/macros")
               (:file "utils/define-query")
               (:file "utils/prinr-to-string")
               (:file "utils/verify-arguments")
               ;; INSTALL
               (:file "install/install")
               ;; PROTOCOLS - MIXINS
               (:file "protocols/serializable")
               (:file "protocols/default-deserializable")
               (:file "protocols/named")
               (:file "protocols/killable")
               ;; PROTOCOLS - CLASSES
               (:file "protocols/date")
               ;; CLASSES
               (:file "classes/standard-date")
               ))
