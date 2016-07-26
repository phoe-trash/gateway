;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-password.lisp

(in-package #:gateway)

(defclass standard-password (password)
  ((%key :reader key-of
	 :type string)
   (%salt :reader salt
	  :type string) 
   (%iteration-count :reader iteration-count
		     :type integer)))

(defconstructor (standard-password passphrase) 
  (check-type passphrase string)
  (setf (values (slot-value standard-password '%key)
		(slot-value standard-password '%salt)
		(slot-value standard-password '%iteration-count))
	(derive-key passphrase)))

(defmethod password-matches-p ((password standard-password) (passphrase string))
  (let* ((salt (ironclad:hex-string-to-byte-array (salt password)))
	 (key (derive-key passphrase
			  :salt salt
			  :iteration-count (iteration-count password)
			  :key-length (key-length password))))
    (string= key (key-of password))))

(defmethod sexp ((password standard-password))
  `(:password :key ,(key-of password)
	      :salt ,(salt password)
	      :iters ,(iteration-count password)))

(defun derive-key (passphrase &key salt iteration-count key-length)
  (let* ((kdf (ironclad:make-kdf 'ironclad:scrypt-kdf :digest :sha1))
	 (passphrase (flex:string-to-octets passphrase :external-format :utf-8))
	 (salt (or salt (ironclad:make-random-salt *password-salt-length*)))
	 (iteration-count (or iteration-count *password-iteration-count*))
	 (key-length (or key-length *password-key-length*))
	 (key (ironclad:derive-key kdf passphrase salt iteration-count key-length))
	 (result-key (ironclad:byte-array-to-hex-string key))
	 (result-salt (ironclad:byte-array-to-hex-string salt)))
    (values result-key result-salt iteration-count)))

(defun key-length (password)
  (/ (length (key-of password)) 2))

(defun salt-length (password)
  (/ (length (salt password)) 2))

(defmethod make-password ((passphrase string))
  (check-type passphrase string)
  (make-instance 'standard-password :passphrase passphrase))
