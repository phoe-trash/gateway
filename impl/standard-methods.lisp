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
  object)

(defun data-equal (object-1 object-2)
  (cond ((and (consp object-1) (consp object-2))
	 (every #'data-equal object-1 object-2))
	((and (symbolp object-1) (symbolp object-2))
	 (string= object-1 object-2))
	(t
	 (equal object-1 object-2))))

;; ;; PARSE
;; ;; TODO: refactor this into a full parser, elsewhere
;; (defun %parse (object &optional parent)
;;   (when (typep (first object) '%quasisymbol)
;;     (setf object (%dequasify object)))
;;   (unless (null object)
;;     (destructuring-bind (identifier . data) object
;;       (when (symbolp identifier)
;;         (etypecase parent
;;           (crown (switch (identifier :test #'string=)
;;                    (:date (parse-date (string=-getf data :date))) 
;;                    (:password (%parse-password data)) 
;;                    (:player (identify :player (string=-getf data :name))) 
;;                    (t object)))
;;           (jewel (switch (identifier :test #'string=)
;;                    (:date (parse-date (string=-getf data :date)))
;;                    (:message (%parse-message data)) 
;;                    (:chat (identify :chat (string=-getf data :name))) 
;;                    (:persona (identify :persona (string=-getf data :name)))
;;                    (t object)))
;;           (null (switch (identifier :test #'string=)
;; 		  (:date (parse-date (string=-getf data :date)))
;; 		  (:message (%parse-message data))
;; 		  (:password (%parse-password data))
;; 		  (:chat (identify :chat (string=-getf data :name)))
;; 		  (:player (identify :player (string=-getf data :name)))
;; 		  (:persona (identify :persona (string=-getf data :name)))
;; 		  (t (format t "[!] %PARSE: CASE failed: ~S.~%" identifier)
;; 		     object))))))))

;; (defun %parse-message (body)
;;   (let ((sender (string=-getf body :sender))
;; 	(recipient (string=-getf body :recipient))
;; 	(date (string=-getf body :date))
;; 	(contents (string=-getf body :contents)))
;;     (msg (parse sender)
;; 	 (parse recipient) 
;; 	 contents
;; 	 :date (parse date))))

;; (defun %parse-password (body)
;;   (let ((key (string=-getf body :key))
;; 	(salt (string=-getf body :salt))
;; 	(iters (string=-getf body :iters)))
;;     (make-instance 'standard-password
;; 		   :%read-data (list key salt iters))))

;; ;; IDENTIFY
;; (defun %identify (type key)
;;   (multiple-value-bind (value found-p) (cache type key) 
;;     (if found-p
;; 	value
;; 	(format t "[!] %IDENTIFY: not found: ~S ~S.~%" type key))))

;; ;; CACHE
;; (defun %cache (type key)
;;   (declare (special *cache-lock* *cache-list*))
;;   (with-lock-held (*cache-lock*)
;;     (gethash key (gethash type *cache-list*))))

;; (defun (setf %cache) (new-value type key)
;;   (declare (special *cache-lock* *cache-list*))
;;   (with-lock-held (*cache-lock*)
;;     (setf (gethash key (gethash type *cache-list*)) new-value)))
