;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/package.lisp

(uiop:define-package #:gateway/utils
  (:use #:common-lisp
        #:alexandria
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
