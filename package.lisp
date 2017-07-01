;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:alexandria))

(defpackage #:gateway/utils
  (:use #:common-lisp
        #:alexandria)
  (:export #:in-directory
           #:*current-directory*
           #:define-query))

(defpackage #:gateway/sql
  (:use #:common-lisp
        #:alexandria
        #:postmodern

        #:gateway/utils))
