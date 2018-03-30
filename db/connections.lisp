;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; db/connections.lisp

(in-package #:gateway/db)

(defvar *db-connection* nil
  "The connection to the database used for production.")

(defvar *test-db-connection* nil
  "The connection to the database used for tests.
DO NOT USE this variable for production databases - running the tests will drop
this database and create it anew.")

(defun connect-test-db ()
  "Connects the test database and returns the connection object. Returns no
values.

Signals an error if the database is already connected with a restart offering
to reconnect to the database."
  (restart-case
      (when (and (typep *db-connection* 'postmodern:database-connection)
                 (postmodern:connected-p *test-db-connection*))
        (error "Test database is already connected."))
    (continue ()
      :report "Disconnect and reconnect the database."
      (postmodern:disconnect *test-db-connection*)
      (postmodern:reconnect *test-db-connection*)
      (return-from connect-test-db (values))))
  (let* ((database (config :test-db-name))
         (user (config :test-db-user))
         (pass (config :test-db-pass))
         (host (config :test-db-host))
         (port (config :test-db-port))
         (use-ssl (multiple-value-bind (value foundp) (config :test-db-use-ssl)
                    (if foundp value :yes))) ;; TODO use default from protocol
         (connection (postmodern:connect database user pass host
                                         :port port :use-ssl use-ssl)))
    (setf *test-db-connection* connection)
    (values)))

(defun disconnect-test-db ()
  "Disconnects the test database."
  (when (and (typep *db-connection* 'postmodern:database-connection)
             (postmodern:connected-p *test-db-connection*))
    (postmodern:disconnect *test-db-connection*)
    (values)))

(defun test-db-connected-p ()
  "Returns true if the test database is connected, false otherwise."
  (postmodern:connected-p *test-db-connection*))

(defun connect-db ()
  "Connects the database and returns the connection object. Returns no
values.

Signals an error if the database is already connected with a restart offering
to reconnect to the database."
  (restart-case
      (when (and (typep *db-connection* 'postmodern:database-connection)
                 (postmodern:connected-p *db-connection*))
        (error "Database is already connected."))
    (continue ()
      :report "Disconnect and reconnect the database."
      (postmodern:disconnect *db-connection*)
      (postmodern:reconnect *db-connection*)
      (return-from connect-db (values))))
  (let* ((database (config :db-name))
         (user (config :db-user))
         (pass (config :db-pass))
         (host (config :db-host))
         (port (config :db-port))
         (use-ssl (multiple-value-bind (value foundp) (config :test-db-use-ssl)
                    (if foundp value :no)))
         (connection (postmodern:connect database user pass host
                                         :port port :use-ssl use-ssl)))
    (setf *db-connection* connection)
    (values)))

(defun disconnect-db ()
  "Disconnects the database."
  (when (and (typep *db-connection* 'postmodern:database-connection)
             (postmodern:connected-p *db-connection*))
    (postmodern:disconnect *db-connection*)
    (values)))

(defun db-connected-p ()
  "Returns true if the database is connected, false otherwise."
  (postmodern:connected-p *db-connection*))

(defmacro with-db (&body body)
  "Evaluates forms with the database connection bound to the Gateway database."
  `(let ((postmodern:*database* *db-connection*))
     (assert (db-connected-p))
     ,@body))

(defmacro with-test-db (&body body)
  "Evaluates forms with the database connection bound to the Gateway test
database."
  `(let ((postmodern:*database* *test-db-connection*))
     (assert (test-db-connected-p))
     ,@body))
