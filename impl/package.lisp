;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; impl/package.lisp

(uiop:define-package #:gateway/impl
  (:use #:common-lisp
        #:named-readtables
        #:alexandria
        #:closer-mop
        #:cl-cpus
        #:1am
        #:safe-read
        #:usocket
        #:bordeaux-threads
        #:lparallel.queue
        #:protest
        #:phoe-toolbox
        #:gateway/utils
        #:gateway/protocols
        #:gateway/framework)
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function
                          #:defmethod
                          #:defgeneric)
  (:shadowing-import-from #:phoe-toolbox
                          #:with-temp-package)
  (:export #:standard-acceptor
           #:standard-connection
           #:standard-date
           #:standard-kernel
           #:standard-listener
           #:standard-password
           #:standard-socket))

(uiop:define-package #:gateway/tests
  (:use))

(protest:define-test-package #:gateway/impl #:gateway/tests)
