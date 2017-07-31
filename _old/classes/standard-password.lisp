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

(define-constructor (standard-password passphrase %read-data
                                       (iteration-count 1000)
                                       (key-length 512)
                                       (salt-length 64))
  (if %read-data
      (progn (check-type %read-data cons)
             (assert (= (length %read-data) 3))
             (check-type (first %read-data) string)
             (check-type (second %read-data) string)
             (check-type (third %read-data) integer))
      (check-type passphrase string))
  (macrolet
      ((val (slot) `(slot-value standard-password ',slot))
       (bind (place)
         `(setf (values (val %key) (val %salt) (val %iteration-count)) ,place)))
    (if %read-data
        (bind (values-list %read-data))
        (bind (derive-key passphrase :iteration-count iteration-count
                                     :key-length key-length
                                     :salt-length salt-length)))))

(defmethod password-matches-p ((password standard-password) (passphrase string))
  (let* ((salt (ironclad:hex-string-to-byte-array (salt password)))
         (key (derive-key passphrase :salt salt
                                     :iteration-count (iteration-count password)
                                     :key-length (key-length password))))
    (string= key (key-of password))))

(defmethod sexp ((password standard-password))
  (sexp `(#:password #:key ,(key-of password)
                     #:salt ,(salt password)
                     #:iters ,(iteration-count password))))

(defunsexp password ((key #:key) (salt #:salt) (iters #:iters)) ()
  (assert key () "Key cannot be empty.")
  (assert salt () "Salt cannot be empty.")
  (assert iters () "Iterations cannot be empty.")
  (make-instance 'standard-password :%read-data (list key salt iters)))

(defun key-length (password)
  (/ (length (key-of password)) 2))

(defun salt-length (password)
  (/ (length (salt password)) 2))

(defmethod make-password ((passphrase string))
  (check-type passphrase string)
  (make-instance 'standard-password :passphrase passphrase))



(deftest test-standard-password
  ;;;; Commented out for speed, as password hashing takes time.
  ;; (let ((wrong-passphrase "Wr0ng-Pas$w0rd"))
  ;;   (flet ((check-password (passphrase)
  ;;            (let ((password (make-password passphrase)))
  ;;              (is (password-matches-p password passphrase))
  ;;              (is (not (password-matches-p password wrong-passphrase))))))
  ;;     (mapc #'check-password
  ;;             '("" "pass" "password-1" "password-2PassW0RD"
  ;;               "password-2ĄŚÐΩŒĘ®ĘŒ®ÐÆąęea
  ;; ÆŃ±¡¿¾   £¼‰‰ę©œ»æśððæś"))))
  (let ((password (make-password "foo")))
    (is (password-matches-p (unsexp (sexp password)) "foo"))))
