;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; utils/gateway.utils.asd

(asdf:defsystem #:gateway.utils
  :description "Utilities for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:postmodern
               #:ironclad
               #:trivial-arguments
               #:trivia
               #:usocket
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "functions")
               (:file "prinr-to-string")
               (:file "verify-arguments")))
