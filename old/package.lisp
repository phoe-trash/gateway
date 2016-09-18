;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; package.lisp

(defpackage #:gateway
  (:shadowing-import-from :closer-mop
   :standard-generic-function :defmethod :defgeneric)
  (:use #:common-lisp
	#:closer-mop 
	#:alexandria
	#:1am
	#:secure-read
	#:bordeaux-threads
	#:usocket
	#:jpl-queues)
  #|
  (:use #:common-lisp
	#:closer-mop
	#:named-readtables
	#:hu.dwim.defclass-star
	#:cl-colors
	#:jpl-queues
	#:trivial-garbage
	#:bordeaux-threads
	#:alexandria
	#:usocket
	#:flexi-streams
	#:iterate
	#:local-time
	#:1am)
  |#
  )

