;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/connections.lisp

(in-package #:gateway/db)

(defmacro with-db (() &body body)
  "Evaluates forms with the database connection bound to the Gateway database."
  `(%with-db (:db-name :db-user :db-pass :db-host :db-port :db-use-ssl)
     ,@body))

(defmacro with-test-db (() &body body)
  "Evaluates forms with the database connection bound to the Gateway test
database."
  `(%with-db (:test-db-name :test-db-user :test-db-pass :test-db-host
              :test-db-port :test-db-use-ssl)
     ,@body))

(defmacro %with-db
    ((name-var user-var pass-var host-var port-var ssl-var) &body body)
  (with-gensyms (name user pass host port ssl)
    `(let* ((,name (config ,name-var)) (,user (config ,user-var))
            (,pass (config ,pass-var)) (,host (config ,host-var))
            (,port (config ,port-var)) (,ssl (config ,ssl-var :yes)))
       ;; TODO use default :USE-SSL value from protocol instead of hardcoding it
       (postmodern:with-connection '(,name ,user ,pass ,host
                                     :port ,port :use-ssl ,ssl :pooled-p t)
         ,@body))))

(defun test-db-connections ()
  (with-db () (postmodern:query "SELECT 1;"))
  (with-test-db () (postmodern:query "SELECT 1;"))
  t)

;; TODO remove this after https://bugs.launchpad.net/sbcl/+bug/1750466 is fixed
(in-package :postmodern)

(defmacro with-connection (spec &body body &environment env)
  "Locally establish a database connection, and bind *database* to it."
  (let ((connect-form (if (and (constantp spec env)
                               (listp spec)
                               (= 2 (length spec))
                               (eq (first spec) 'quote))
                          `(funcall #'connect ,@(second spec))
                          `(apply #'connect ,spec))))
    `(let ((*database* ,connect-form))
       (unwind-protect (progn ,@body)
         (disconnect *database*)))))
