;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:alexandria))

(defpackage #:gateway/utils
  (:use #:common-lisp
        #:alexandria
        #:postmodern)
  (:export #:define-query
           #:define-queries))

(defpackage #:gateway/install
  (:use #:common-lisp
        #:alexandria
        #:postmodern

        #:gateway/utils))
