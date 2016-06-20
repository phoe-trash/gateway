;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass password ()
  ((%key :accessor key
	 :type string)
   (%salt :accessor salt
	  :type string)))

(defun derive-key (passphrase &optional salt)
  (let* ((kdf (ironclad:make-kdf 'ironclad:scrypt-kdf :digest :sha1))
	 (passphrase (flex:string-to-octets passphrase :external-format :utf-8))
	 (salt (or salt (ironclad:make-random-salt *password-salt-length*)))
	 (iteration-count *password-iteration-count*)
	 (key-length *password-key-length*) 
	 (key (ironclad:derive-key kdf passphrase salt iteration-count key-length))
	 (result-key (ironclad:byte-array-to-hex-string key))
	 (result-salt (ironclad:byte-array-to-hex-string salt)))
    (values result-key result-salt)))

(defconstructor (password passphrase)
  (when (null passphrase)
    (error "PASSPHRASE not provided."))
  (check-type passphrase string)
  (setf (values (key password) (salt password)) (derive-key passphrase)))

(defmethod make-password ((passphrase string))
  (make-instance 'password :passphrase passphrase))

(defmethod password-matches-p ((password password) (passphrase string))
  (let* ((salt (ironclad:hex-string-to-byte-array (salt password)))
	 (key (derive-key passphrase salt)))
    (string= key (key password))))
