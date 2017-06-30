;;;; GATEWAY
(asdf:defsystem #:gateway
  :description "A graphical chat/RP client written in Common Lisp."
  :author "Michał \"phoe\" Herda"
  :license "GPL3"
  :depends-on (#:alexandria
               #:cl-yesql
               #:postmodern
               #:closer-mop
               ;; #:ironclad
               ;; #:flexi-streams
               ;; #:local-time
               ;; #:1am
               ;; #:secure-read
               ;; #:bordeaux-threads
               ;; #:usocket
               ;; #:lparallel
               ;; #:cl-ppcre
               ;; #:trivial-arguments
               ;; #:trivia
               ;; #:bknr.datastore
               ;;#:split-sequence
               )
  :serial t
  :components (;;PACKAGE
               (:file "package")
               ))
