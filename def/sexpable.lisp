;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; sexpable.lisp

(in-package #:gateway)

#|
Protocol SEXPABLE

A class that is sexpable must:

1) Implement SEXP generic function, which serializes instances
of a given class into a S-expression form which only contains:
* symbols (without any keyword information),
* proper lists,
* numbers,
* strings;

2) Provide a parser for the PARSE function which takes a
S-expression (such as the one output from the SEXP generic
function) and returns an object of a given class equivalent to
the original.
|#
(defprotocol sexpable ()
  (defgeneric sexp (object))
  (defun unsexp (sexp &optional parent)
    (%unsexp sexp parent)))

