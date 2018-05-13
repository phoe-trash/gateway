;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/gateway-config.lisp
;; TODO change the above from protocols/ to protocol/

(in-package #:gateway/protocol)

(define-protocol gateway-config
    (:documentation "The GATEWAY-CONFIG protocol describes all configuration
entries required for proper functioning of Gateway."
     :tags (:config :gateway)
     :export nil)
  ;; TODO add option to specify callbacks here or something
  (:category (:gateway :db))
  "This config category describes configuration settings that regard ~
connecting to a PostgreSQL database. Providing the connections details ~
is mandatory for Gateway to function."
  (:config (:db :db-name) string :mandatory)
  "The database name for the database connection."
  (:config (:gateway :db :db-user) string :mandatory)
  "The username for the database connection."
  (:config (:gateway :db :db-pass) string :mandatory)
  "The password for the database connection."
  (:config (:gateway :db :db-host) string :mandatory)
  "The hostname for the database connection."
  (:config (:gateway :db :db-port) (unsigned-byte 16) :mandatory)
  "The port for the database connection."
  (:config (:gateway :db :db-use-ssl) (member :yes :no :try) :optional :yes)
  "Should the DB connection use SSL?"
  (:category (:gateway :test-db))
  "This config category describes configuration settings that regard ~
connecting to a PostgreSQL test database. It is a scrap database for running ~
the database tests from the Gateway test suite.
\
This category is optional, but if you do not provide it, you will not be able ~
to run the full suite of Gateway tests."
  (:config (:gateway :test-db :test-db-name) string :optional)
  "The database name for the test database connection."
  (:config (:gateway :test-db :test-db-user) string :optional)
  "The username for the test database connection."
  (:config (:gateway :test-db :test-db-pass) string :optional)
  "The password for the test database connection."
  (:config (:gateway :test-db :test-db-host) string :optional)
  "The hostname for the test database connection."
  (:config (:gateway :test-db :test-db-port) (unsigned-byte 16) :optional)
  "The port for the test database connection."
  (:config (:gateway :test-db :test-db-use-ssl) (member :yes :no :try)
           :optional :no)
  "Should the test database connection use SSL?"
  (:category (:gateway :performance))
  "This config category describes configuration settings that regard runtime ~
performance of the server."
  (:config (:gateway :performance :kernel-threads) (integer 1) :optional
           #.(cl-cpus:get-number-of-processors))
  "The number of kernel threads Gateway should spawn. Should default to the
number of logical processors available on the machine.")

(execute-protocol gateway-config)
