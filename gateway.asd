;;;; Autogenerated ASD file for system "GATEWAY"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(asdf/parse-defsystem:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "GPL3"
  :depends-on (#:alexandria
               #:closer-mop
               #:ironclad
               #:flexi-streams
               #:local-time
               #:1am
               #:secure-read
               #:bordeaux-threads
               #:usocket
               #:lparallel
               #:trivial-arguments
               #:trivia)
  :serial t
  :components (;; PACKAGE
               (:file "package")
               ;; UTIL
               (:file "util/macros")
               (:file "util/functions")
               ;; DEF
               (:file "def/sexpable")
               (:file "def/killable")
               (:file "def/named")
               (:file "def/date")
               (:file "def/password")
               (:file "def/library")
               (:file "def/connection")
               (:file "def/acceptor")
               (:file "def/listener")
               (:file "def/timer")
               (:file "def/crown")
               ;; IMPL
               (:file "impl/sexp")
               (:file "impl/standard-date")
               (:file "impl/standard-password")
               (:file "impl/standard-library")
               (:file "impl/standard-socket")
               (:file "impl/standard-connection")
               (:file "impl/standard-acceptor")
               (:file "impl/standard-listener")
               (:file "impl/standard-crown")
               ;; OTHER
               (:file "new")))
