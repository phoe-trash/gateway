;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/config.lisp

(in-package :gateway/protocols)

(export 'config)

(define-protocol config
    (:description "The CONFIG protocol describes the contents of a Gateway ~
configuration file.

The contents of a configuration file is a single plist in form of (:FOO :BAR ~
:BAZ \"quux\" :FRED 123 ...) with some options mandatory and some being ~
optional. These settings describe various aspects of Gateway's functioning, ~
from the database connection details to the number of kernel threads that ~
Gateway should occupy for its functioning."
     :tags (:config)
     :export nil)
  (:category :db)
  "This config category describes configuration settings that regard ~
connecting to a PostgreSQL database. Providing the connections details ~
is mandatory for Gateway to function."
  (:option :db :db-name string :mandatory)
  "The database name for the database connection."
  (:option :db :db-user string :mandatory)
  "The username for the database connection."
  (:option :db :db-password string :mandatory)
  "The password for the database connection."
  (:option :db :db-host string :mandatory)
  "The hostname for the database connection."
  (:option :db :db-port (unsigned-byte 16) :mandatory)
  "The port for the database connection."
  (:option :db :db-use-ssl boolean :optional t)
  "Should the DB connection use SSL?"
  (:category :test-db)
  "This config category describes configuration settings that regard ~
connecting to a PostgreSQL test database. It is a scrap database for running ~
the database tests from the Gateway test suite.

This category is optional, but if you do not provide it, you will not be able ~
to run the full suite of Gateway tests."
  (:option :test-db :test-db-name string :optional)
  "The database name for the test database connection."
  (:option :test-db :test-db-user string :optional)
  "The username for the test database connection."
  (:option :test-db :test-db-password string :optional)
  "The password for the test database connection."
  (:option :test-db :test-db-host string :optional)
  "The hostname for the test database connection."
  (:option :test-db :test-db-port (unsigned-byte 16) :optional)
  "The port for the test database connection."
  (:option :test-db :test-db-use-ssl boolean :optional t)
  "Should the test database connection use SSL?"
  (:category :performance)
  "This config category describes configuration settings that regard runtime ~
performance of the server."
  (:option :performance :kernel-threads (integer 1) :optional
           "the total number of logical processor available on the server")
  "The number of kernel threads Gateway should spawn.")
