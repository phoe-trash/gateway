;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:gateway
  (:use #:common-lisp
        #:closer-mop
        #:alexandria
        #:secure-read
        #:bordeaux-threads
        #:usocket
        #:trivial-arguments
        #:trivia
        #:cl-ppcre
        #:lparallel
        #:lparallel.queue
        #:bknr.datastore)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function #:defmethod #:defgeneric
                          #:standard-method #:standard-class)
  (:shadowing-import-from #:1am
                          #:is #:signals #:*tests*))
