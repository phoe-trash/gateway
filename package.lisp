;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:alexandria))

(defpackage #:gateway/utils
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
   #:peek-char-no-hang
   #:data-getf
   #:data-equal
   #:valid-email-p
   #:valid-username-p
   #:valid-name-p
   #:fformat
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

(defpackage #:gateway/framework
  (:use #:common-lisp
        #:gateway/utils
        #:gateway/protocols)
  (:export
   ;; WITH-RESTARTABILITY
   #:with-restartability
   ))

(defpackage #:gateway/install
  (:use #:common-lisp
        #:postmodern
        #:gateway/utils)
  (:export #:install
           #:uninstall
           #:reload))

(uiop:define-package #:gateway/protocols
    (:use #:common-lisp
          #:closer-mop
          #:protest
          #:gateway/utils
          #:gateway/framework)
  (:shadowing-import-from #:protest
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(defpackage #:gateway/impl
  (:use #:common-lisp
        #:named-readtables
        #:alexandria
        #:closer-mop
        #:1am
        #:safe-read
        #:usocket
        #:bordeaux-threads
        #:protest
        #:gateway/utils
        #:gateway/protocols)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(uiop:define-package #:gateway/tests
    (:use))

(protest:define-test-package #:gateway/impl #:gateway/tests)
