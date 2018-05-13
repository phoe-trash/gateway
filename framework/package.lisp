;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/package.lisp

(defpackage #:gateway/framework
  (:use #:common-lisp
        #:phoe-toolbox
        #:gateway/protocol)
  (:export
   #:with-restartability))
