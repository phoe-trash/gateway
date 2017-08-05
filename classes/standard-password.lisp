;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; classes/standard-password.lisp

(in-package :gateway/impl)

(defclass standard-password (password)
  ((%key :reader key-of
         :type string)
   (%salt :reader salt
          :type string)
   (%iteration-count :reader iteration-count
                     :type integer))
  (:description #.(format nil "A standard implementation of a Gateway protocol ~
class PASSWORD.

STANDARD-PASSWORD instances are constructible by two means: by providing the ~
passphrase (and optionally other password details) or by providing the ~
deserialized password data.")))

(define-constructor (standard-password passphrase deserialized-data
                                       (iteration-count 1000)
                                       (key-length 512)
                                       (salt-length 64))
  (macrolet ((val (slot) `(slot-value standard-password ',slot))
             (bind (place)
               `(setf (values (val %key) (val %salt) (val %iteration-count))
                      ,place)))
    (if deserialized-data
        (progn (check-type deserialized-data cons)
               (assert (= (length deserialized-data) 3))
               (check-type (first deserialized-data) string)
               (check-type (second deserialized-data) string)
               (check-type (third deserialized-data) integer)
               (bind (values-list deserialized-data)))
        (progn (check-type passphrase string)
               (bind (derive-key passphrase
                       :iteration-count iteration-count
                       :key-length key-length
                       :salt-length salt-length))))))

(defmethod serialize ((object standard-password) &key (type :list))
  (let ((data `(:password :key ,(key-of object)
                          :salt ,(salt object)
                          :iteration-count ,(iteration-count object))))
    (ecase type
      (:list data)
      (:string (prinr-to-string data)))))

(defmethod deserialize-using-class
    ((class (eql (find-class 'standard-password))) data)
  (check-type data cons)
  (assert (= (length data) 7))
  (destructuring-bind (symbol . plist) data
    (check-type symbol symbol)
    (assert (string= symbol :password))
    (let ((key (data-getf plist :key))
          (salt (data-getf plist :salt))
          (iteration-count (data-getf plist :iteration-count)))
      (check-type key string)
      (check-type salt string)
      (check-type iteration-count unsigned-byte)
      (make-instance 'standard-password
                     :deserialized-data (list key salt iteration-count)))))

(defmethod password-matches-p ((password standard-password) (passphrase string))
  (let* ((salt (ironclad:hex-string-to-byte-array (salt password)))
         (key (derive-key passphrase :salt salt
                                     :iteration-count (iteration-count password)
                                     :key-length (key-length password))))
    (string= key (key-of password))))

(defun derive-key (passphrase &key salt
                                (iteration-count 1000)
                                (key-length 512)
                                (salt-length 64))
  (let* ((kdf (ironclad:make-kdf 'ironclad:scrypt-kdf :digest :sha1))
         (passphrase (flex:string-to-octets passphrase :external-format :utf-8))
         (salt (or salt (ironclad:make-random-salt salt-length)))
         (key (ironclad:derive-key kdf passphrase salt
                                   iteration-count key-length))
         (result-key (ironclad:byte-array-to-hex-string key))
         (result-salt (ironclad:byte-array-to-hex-string salt)))
    (values result-key result-salt iteration-count)))

(defun key-length (password)
  (/ (length (key-of password)) 2))

(defun salt-length (password)
  (/ (length (salt password)) 2))

;;; TESTS

(define-test-case standard-password-unit
    (:description "Unit tests for STANDARD-PASSWORD."
     :tags (:unit :password :long-test)
     :type :unit-suite))

(define-test standard-password-unit
  (let ((wrong-passphrase "Wr0ng-Pas$w0rd"))
    (flet ((check-password (passphrase)
             (let ((password (make-instance 'standard-password
                                            :passphrase passphrase)))
               (is (password-matches-p password passphrase))
               (is (not (password-matches-p password wrong-passphrase))))))
      (mapc #'check-password
            '("" "pass" "password-1" "password-2PassW0RD"
              "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆąęea
    ÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))))
  (let* ((password (make-instance 'standard-password
                                  :passphrase "foo"))
         (serialized (serialize password))
         (deserialized (deserialize-using-class (find-class 'standard-password)
                                                serialized)))
    (is (password-matches-p deserialized "foo"))))
