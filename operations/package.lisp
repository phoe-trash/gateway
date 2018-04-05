;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; operations/package.lisp

(defpackage #:gateway/operations
  (:use #:common-lisp
        #:alexandria
        #:phoe-toolbox
        #:gateway/protocols
        #:gateway/sql)
  (:export #:install
           #:uninstall
           #:reinstall))
