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
           #:data-getf))

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
        #:cl-protest)
  (:shadowing-import-from #:cl-protest
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))

(defpackage #:gateway/impl
  (:use #:common-lisp
        #:alexandria
        #:closer-mop
        #:gateway/utils
        #:gateway/protocols)
  (:shadowing-import-from #:cl-protest
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric))
