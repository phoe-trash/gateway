;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-methods.lisp

(in-package #:gateway)

;; SEXP

(defmethod sexp (object)
  (format t "[!] SEXP: no method for ~S. Bug?~%" object)
  object)

(defmethod sexp ((object list))
  (mapcar #'sexp object))

(defmethod sexp ((object integer))
  object)

(defmethod sexp ((object string))
  object)

(defmethod sexp ((object symbol))
  (make-instance '%quasisymbol :symbol object))
(defclass %quasisymbol () ((symbol :initarg :symbol :accessor s)))
(defprint %quasisymbol (princ (symbol-name (s obj)) stream))

;; PARSE

(defun %parse (object)
  (destructuring-bind (identifier . data) object
    (switch (identifier :test #'string=)
      (:date (parse-date (string=-getf data :date)))
      (:message (%parse-message data))
      (:password (%parse-password data))
      (:chat (identify :chat (string=-getf data :name)))
      (:player (identify :player (string=-getf data :name)))
      (:persona (identify :persona (string=-getf data :name)))
      (t (format t "[!] %PARSE: CASE failed: ~S.~%" identifier)
	 object))))

(defun %parse-message (body)
  (let ((sender (string=-getf body :sender))
	(recipient (string=-getf body :recipient))
	(date (string=-getf body :date))
	(contents (string=-getf body :contents)))
    (msg (parse sender)
	 (parse recipient) 
	 contents
	 :date (parse date))))

(defun %parse-password (body)
  (let ((key (string=-getf body :key))
	(salt (string=-getf body :salt))
	(iters (string=-getf body :iters)))
    (make-instance 'standard-password
		   :%read-data (list key salt iters))))

;; IDENTIFY

(defun %identify (type key)
  (multiple-value-bind (value found-p) (cache type key) 
    (if found-p
	value
	(format t "[!] %IDENTIFY: not found: ~S ~S.~%" type key)))
  #|
  (flet ((ensure (key hash-table)
	   (multiple-value-bind (value foundp) (gethash key hash-table)
	     (if foundp
		 value
		 (format t "[!] IDENTIFY: not found: ~S ~S.~%" type key))))) 
    (case type
      (:chat (ensure key *chat-cache*))
      (:player (ensure key *player-cache*))
      (:persona (ensure key *persona-cache*))
      (t (format t "[!] %IDENTIFY: CASE failed: ~S.~%" type))))
    |#)

;; CACHE

(defun %cache (type key)
  (with-lock-held (*cache-lock*)
    (gethash key (gethash type *cache-list*))))

(defun (setf %cache) (new-value type key)
  (with-lock-held (*cache-lock*)
    (setf (gethash key (gethash type *cache-list*)) new-value)))
