;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:closer-mop
        #:alexandria
        #:cl-yesql)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function #:defmethod #:defgeneric
                          #:standard-method #:standard-class))
