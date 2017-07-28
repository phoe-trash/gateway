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
        #:postmodern)
  (:export #:define-query
           #:define-queries
           #:count-digits
           #:data-getf
           #:prinr-to-string))

(defpackage #:gateway/install
  (:use #:common-lisp
        #:postmodern
        #:gateway/utils)
  (:export #:install
           #:uninstall
           #:reload))

(defpackage #:gateway/protocols
  (:use #:common-lisp
        #:closer-mop
        #:protest)
  (:shadowing-import-from #:protest
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(defpackage #:gateway/impl
  (:use #:common-lisp
        #:alexandria
        #:closer-mop
        #:1am
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
