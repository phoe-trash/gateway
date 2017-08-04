;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; protocols/kernel.lisp

(in-package :gateway/protocols)

(define-protocol kernel
    (:description "The KERNEL protocol describes objects which are capable of ~
processing messages, either sequentially or concurrently. In the second case, ~
the kernel contains implementation-dependent objects called workers.

The message's format is implementation-dependent.

On each enqueued message, the kernel's handler function is eventually called ~
on that message. That function is an one-argument function that expects the ~
implementation-dependent message as its argument."
     :tags (:kernel)
     :dependencies (killable named)
     :export t)
  (:class kernel (killable named) ())
  ;; TODO change this everywhere to "see protocol X for details"
  "A kernel object."
  (:function enqueue ((kernel kernel) message) (values))
  "Enqueues a message in the kernel for processing. Returns T if the push was ~
successful, NIL otherwise."
  (:function handler ((kernel kernel)) function)
  "Returns the kernel's handler function."
  (:function (setf handler) (new-value (kernel kernel)) new-value)
  "Sets the kernel's handler function.")
