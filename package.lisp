;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:alexandria))

(uiop:define-package #:gateway/utils
    (:use #:common-lisp
          #:alexandria
          #:postmodern
          #:trivial-arguments
          #:trivia
          #:cl-ppcre)
  (:export
   ;; FUNCTIONS
   #:count-digits
   #:cat
   #:catn
   #:peek-char-no-hang
   #:data-getf
   #:data-equal
   #:valid-email-p
   #:valid-username-p
   #:valid-name-p
   #:fformat
   #:pprint-plist
   ;; MACROS
   #:define-constructor
   #:define-print
   #:wait
   #:wait-until
   #:finalized-let*
   ;; DEFINE-QUERY
   #:define-query
   #:define-queries
   ;; PRINR-TO-STRING
   #:prinr-to-string
   ;; VERIFY-ARGUMENTS
   #:verify-arguments
   ))

(defpackage #:gateway/variables
  (:use #:common-lisp)
  (:export
   ;; CONSTANTS
   #:*date-granularity-units*))

(uiop:define-package #:gateway/protocols
    (:use #:common-lisp
          #:closer-mop
          #:protest
          #:gateway/utils)
  (:shadowing-import-from #:protest
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(defpackage #:gateway/framework
  (:use #:common-lisp
        #:gateway/utils
        #:gateway/protocols)
  (:export
   ;; WITH-RESTARTABILITY
   #:with-restartability
   ))

(defpackage #:gateway/config
  (:use #:common-lisp
        #:alexandria
        #:gateway/utils
        #:gateway/protocols))

(defpackage #:gateway/db
  (:use #:common-lisp
        #:cl-yesql
        #:gateway/utils
        #:gateway/config)
  (:export #:connect-db
           #:db-connected-p
           #:disconnect-db
           #:connect-test-db
           #:test-db-connected-p
           #:disconnect-test-db
           #:with-db
           #:with-test-db))

(defpackage #:gateway/install
  (:use #:common-lisp
        #:cl-yesql
        #:gateway/db)
  (:export #:install
           #:uninstall
           #:reload))

(uiop:define-package #:gateway/impl
    (:use #:common-lisp
          #:named-readtables
          #:alexandria
          #:closer-mop
          #:cl-cpus
          #:1am
          #:safe-read
          #:usocket
          #:bordeaux-threads
          #:lparallel.queue
          #:protest
          #:gateway/utils
          #:gateway/variables
          #:gateway/protocols
          #:gateway/framework)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:export #:standard-acceptor
           #:standard-connection
           #:standard-date
           #:standard-kernel
           #:standard-listener
           #:standard-password
           #:standard-socket))

(uiop:define-package #:gateway/tests
    (:use))

(protest:define-test-package #:gateway/impl #:gateway/tests)
