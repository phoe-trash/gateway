;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/package.lisp

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
