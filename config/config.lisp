;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; config/config.lisp

(in-package #:gateway/config)

(defvar *config* nil
  "The current Gateway configuration.")

(defparameter *config-error*
  "The Gateway configuration is invalid:")

(defparameter *keyword-missing*
  "The keyword ~A is missing.")

(defparameter *keyword-wrong-type*
  "The value of keyword ~A is not of expected type ~A.")

(defvar *malformed-config*
  "The configuration is malformed.~%~A")

(defun default-config-path ()
  "Returns the default pathname to the Gateway configuration file, based ~
on the value of USER-HOMEDIR-PATHNAME."
  (merge-pathnames (make-pathname :directory '(:relative ".gateway")
                                  :name "config"
                                  :type "sexp")
                   (user-homedir-pathname)))

(defun load-config (&optional config-path)
  "Loads the Gateway configuration file from the provided path, validates it ~
and sets it as the current Gateway configuration. If no path is provided, the ~
default path is used."
  (let* ((path (or config-path (default-config-path))))
    (with-input-from-file (stream path :if-does-not-exist :error)
      (let ((config (read stream)))
        (validate-config config)
        (setf *config* config)))))

(defun save-config (&optional config-path)
  "Saves the current Gateway config to the provided path, overwriting any ~
previous contents. If no path is provided, the default path is used."
  (let* ((path (or config-path (default-config-path))))
    (with-output-to-file (stream path :if-does-not-exist :create
                                      :if-exists :overwrite)
      (print *config* stream)
      (fresh-line stream))))

(defun validate-config (config &optional (errorp t))
  "Validates the configuration file by asserting that all mandatory options ~
are provided and of proper type.

If ERRORP is true, an error is raised if the configuration does not pass ~
validation. Otherwise, (NIL STRINGS) is returned, where STRINGS is a list of ~
errors detected in the configuration file."
  (let* ((protocols protest:*protocols*)
         (protocol (find 'config protocols :key #'car))
         (forms (cddr protocol))
         (options (remove-if-not (curry #'eq :option) forms :key #'car))
         (mandatory (remove-if (curry #'eq :optional) options :key #'fifth))
         (list (mapcar (lambda (x) (list (third x) (fourth x))) mandatory))
         (errors '()))
    (flet ((epush (control &rest args)
             (push (apply #'format nil control args) errors)))
      (handler-case
          (loop for (keyword type) in list
                unless (get-properties config (list keyword))
                  do (epush *keyword-missing* keyword)
                else unless (typep (getf config keyword) type)
                       do (epush *keyword-wrong-type* keyword type))
        (error (e)
          (epush *malformed-config* e)))
      (cond ((null errors)
             (values t nil))
            (errorp
             (error (apply #'catn *config-error* (nreverse errors))))
            (t
             (values nil (nreverse errors)))))))
