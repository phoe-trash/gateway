;;;; GATEWAY
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
               #:asdf)
  :serial t
  :components (;; PACKAGE
               (:file "package")
               ;; MACROS
               (:file "utils/define-query")
               (:file "utils/varia")
               ;; INSTALL
               (:file "install/install")
               ;; PROTOCOLS
               (:file "protocols/serializable")
               (:file "protocols/default-deserializable")
               (:file "protocols/date")
               ;; IMPLEMENTATIONS
               (:file "classes/standard-date")
               ))
