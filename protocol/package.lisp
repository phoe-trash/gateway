;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; protocols/package.lisp

(uiop:define-package #:gateway/protocol
  (:use #:common-lisp
        #:closer-mop
        #:moptilities
        #:phoe-toolbox
        #:gateway/utils)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:import-from #:protest/protocol
                #:define-protocol
                #:execute-protocol))
