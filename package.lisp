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
        #:usocket
        #:cl-ppcre)
  (:export
   #:data-getf
   #:data-equal
   #:pprint-plist
   #:prinr-to-string
   #:verify-arguments
   #:socket-local-address
   #:socket-peer-address))

(uiop:define-package #:gateway/protocols
  (:use #:common-lisp
        #:closer-mop
        #:protest
        #:phoe-toolbox
        #:gateway/utils)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(defpackage #:gateway/framework
  (:use #:common-lisp
        #:phoe-toolbox
        #:gateway/protocols)
  (:export
   ;; WITH-RESTARTABILITY
   #:with-restartability
   ))

(defpackage #:gateway/config
  (:use #:common-lisp
        #:alexandria
        #:phoe-toolbox
        #:gateway/utils
        #:gateway/protocols))

(defpackage #:gateway/db
  (:use #:common-lisp
        #:alexandria
        #:cl-yesql
        #:phoe-toolbox
        #:gateway/protocols
        #:gateway/config)
  (:export #:with-db
           #:with-test-db))

(defpackage #:gateway/install
  (:use #:common-lisp
        #:cl-yesql
        #:gateway/utils
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
        #:phoe-toolbox
        #:gateway/utils
        #:gateway/protocols
        #:gateway/framework)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:shadowing-import-from #:phoe-toolbox
                          #:with-temp-package)
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
