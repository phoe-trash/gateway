;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(define-condition get-sexp-error (error) ())

(defmacro defsexp (classes &body body)
  (flet ((%defsexp (class &rest body)
	   (list* 'defmethod 'get-sexp `((object ,class)) (car body))))
    (cons 'progn (mapcar (lambda (x) (%defsexp x body)) classes))))

(defsexp (t)
  (todo "DEFSEXP called with default value. Something not implemented yet?"))

(defsexp (integer string null symbol)
  object)

(defsexp (array password world-map connection)
  (error 'get-sexp-error))

(defsexp (list)
  (mapcar #'get-sexp object))

(defsexp (message)
  (list :message :sender (get-sexp (sender object))
		 :recipient (get-sexp (recipient object))
		 :date (get-sexp (date-of object))
		 (get-sexp (contents object))))

(defsexp (persona)
  (list :persona :id (get-sexp (id object))
		 :name (get-sexp (name object))
		 :player (get-sexp (player object))))

(defsexp (chat)
  (list* :chat :id (get-sexp (id object))
	       :name (get-sexp (name object))
	       (when (shard object)
		 :shard (get-sexp (shard object)))))

(defsexp (player)
  (list :player :id (get-sexp (id object))
		:username (get-sexp (username object))))

(defsexp (shard)
  (list :shard  :id (get-sexp (id object))
		:name (get-sexp (name object))))

