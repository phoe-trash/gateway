;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; db/connections.lisp

(in-package #:gateway/db)

(defvar *test-db-connection* nil
  "The connection to the database used for tests.
DO NOT USE this variable for production databases - running the tests will drop
this database and create it anew.")

(defvar *db-connection* nil
  "The connection to the database used for production.")

(defun connect-test-connection ()
  (postmodern:connect ))
