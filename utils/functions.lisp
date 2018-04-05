;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; utils/functions.lisp

(in-package #:gateway/utils)

;; (defun count-digits (integer)
;;   "Returns the number of digits in an integer, sans any sign."
;;   (if (= 0 integer)
;;       1
;;       (values (ceiling (log (abs integer) 10)))))

;; (defun cat (&rest strings)
;;   "Concatenates targets into a string."
;;   (apply #'concatenate 'string strings))

;; (defun catn (&rest strings)
;;   "Concatenates targets into a string, inserting a newline between each of
;; them."
;;   (let ((strings (loop for cons on strings collect (car cons)
;;                        when (cdr cons) collect #.(format nil "~%"))))
;;     (apply #'concatenate 'string strings)))

;; (defun peek-char-no-hang (&optional (input-stream *standard-input*)
;;                             (eof-error-p t) eof-value recursive-p)
;;   "Like PEEK-CHAR, except it returns NIL if there is no character waiting on the
;; provided stream."
;;   (let ((character (read-char-no-hang input-stream eof-error-p
;;                                       eof-value recursive-p)))
;;     (when character
;;       (unread-char character input-stream)
;;       character)))

(defun data-getf (plist indicator)
  "Acts like GETF, except its keys must be symbols that are STRING= to the
INDICATOR."
  (loop for key in plist by #'cddr
        for value in (rest plist) by #'cddr
        when (and (symbolp key) (string= key indicator))
          return value))

(defun data-equal (object-1 object-2)
  "Acts like EQUAL, but returns true for symbols if they are STRING=."
  (cond ((and (consp object-1) (consp object-2))
         (every #'data-equal object-1 object-2))
        ((and (symbolp object-1) (symbolp object-2))
         (string= object-1 object-2))
        (t
         (equal object-1 object-2))))

;; (defun valid-email-p (string)
;;   "Returns true if a string is a (possibly) valid email address.
;; Full RFC validation is not implemented - this is only a basic, rough ."
;;   (scan "^[a-zA-Z0-9-._]+@.*\..*$" string))

;; (defun valid-username-p (string)
;;   "Returns true if a string is a valid username. "
;;   (scan "(?=^.{3,64}$)^[a-zA-Z0-9]+[a-zA-Z0-9._-]*[a-zA-Z0-9]+$" string))

;; (defun valid-name-p (string)
;;   "Returns true if a string is a valid name."
;;   (scan "(?=^.{3,64}$)^[a-zA-Z0-9'\"]+[a-zA-Z0-9'\"._-]*[a-zA-Z0-9'\"]+$"
;;         string))

;; (defun fformat (stream format-string &rest format-args)
;;   "Like FORMAT, except it adds a fresh line after the formatted string and calls
;; FORCE-OUTPUT on the stream after formatting."
;;   (apply #'format stream format-string format-args)
;;   (fresh-line stream)
;;   (force-output stream))

(defun pprint-plist (*standard-output* list)
  "Pretty-prints a plist with newlines after each key-value pair."
  (pprint-logical-block (*standard-output* list :prefix "(" :suffix ")")
    (loop for cell on list by #'cddr
          do (write (first cell))
             (write-char #\Space)
             (write (second cell))
             (when (cddr cell)
               (terpri *standard-output*)
               (write-char #\Space)))
    (terpri *standard-output*)))

(defun socket-local-address (socket)
  "Returns a the socket's local address, in format A.B.C.D:E."
  (handler-case
      (format nil "~{~D.~D.~D.~D~}:~D"
              (coerce (get-local-name socket) 'list)
              (get-local-port socket))
    (error () "<error>")))

(defun socket-peer-address (socket)
  "Returns a the socket's peer address, in format A.B.C.D:E."
  (handler-case
      (format nil "~{~D.~D.~D.~D~}:~D"
              (coerce (get-peer-address socket) 'list)
              (get-peer-port socket))
    (error () "<error>")))
