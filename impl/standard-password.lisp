;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; standard-password.lisp

(in-package #:gateway)

(defclass standard-password (password)
  ((%key :accessor key-of
	 :type string)
   (%salt :accessor salt
	  :type string) 
   (%iteration-count :accessor iteration-count
		     :type integer)))

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

(defconstructor (standard-password passphrase) 
  (check-type passphrase string)
  (setf (values (key-of standard-password)
		(salt standard-password)
		(iteration-count standard-password))
	(derive-key passphrase)))

(defmethod password-matches-p ((password standard-password) (passphrase string))
  (let* ((salt (ironclad:hex-string-to-byte-array (salt password)))
	 (key (derive-key passphrase
			  :salt salt
			  :iteration-count (iteration-count password)
			  :key-length (key-length password))))
    (string= key (key-of password))))

(defun make-password (passphrase)
  (check-type passphrase string)
  (make-instance 'standard-password :passphrase passphrase))
