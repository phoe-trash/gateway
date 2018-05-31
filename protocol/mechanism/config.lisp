;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/config.lisp

(in-package #:gateway/protocol)

(define-protocol config
    (:documentation "The CONFIG protocol describes the mechanisms through which
parts of Gateway obtain their configuration."
     :tags (:config)
     :export t)
  (:function config (option &optional default) t)
  "Retrieves the value of the provided configuration option and returns ~
\(VALUES VALUE T). If the option is not found, returns (VALUES DEFAULT NIL)."
  (:function (setf config) (new-value option &optional default) t)
  "Checks if the new value is of proper type for the provided configuration ~
option. If not, an error is signaled; if yes, that value is set to the config.
\
DEFAULT will be evaluated once if provided, but its value is ignored."
  (:macro with-config-transaction (() &body body))
  "Executes BODY within a configuration transaction to avoid multiple disk
writes where only one would otherwise be required.")

(execute-protocol config)
