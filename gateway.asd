;;;; GATEWAY
(asdf:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "GPL3"
  :depends-on (#:alexandria
               #:overlord
               #:cl-yesql
               #:postmodern
               #:asdf)
  :serial t
  :components (;; PACKAGE
               (:file "package")
               ;; MACROS
               (:file "utils/in-directory")
               ;; INSTALL
               (:file "install/install")
               ))
