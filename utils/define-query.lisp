;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; install.lisp

(in-package :gateway/utils)

(defparameter *current-directory* (asdf:system-relative-pathname :gateway ""))

(defmacro in-directory (directory-name)
  (let* ((pathname (asdf:system-relative-pathname :gateway directory-name)))
    `(setf *current-directory* ,pathname)))

(defmacro define-query (symbol)
  (let* ((name (string-downcase (string symbol)))
         (filename (concatenate 'string name ".sql"))
         (relative-pathname (merge-pathnames filename *current-directory*))
         (pathname (asdf:system-relative-pathname :gateway relative-pathname))
         (query (read-file-into-string pathname)))
    `(defprepared ,symbol ,query)))

(defmacro define-queries (directory-path &rest symbols)
  `(let (*current-directory*)
     (in-directory ,directory-path)
     ,@(mapcar (lambda (x) `(define-query ,x)) symbols)
     t))
