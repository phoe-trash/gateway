;;;; package.lisp

(defpackage #:gateway
  (:shadowing-import-from :closer-mop
   :standard-generic-function :defmethod :defgeneric)
  (:use #:common-lisp
	#:closer-mop
	#:named-readtables
	#:hu.dwim.defclass-star
	#:cl-colors
	#:jpl-queues
	#:bordeaux-threads
	#:alexandria
	#:usocket
	#:flexi-streams
	#:iterate
	#:local-time
	#:1am))

