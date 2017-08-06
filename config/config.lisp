;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; config/config.lisp

(in-package #:gateway/config)

;; TODO implement http://shinmera.github.io/ubiquitous/

(defparameter *config-error*
  "The Gateway configuration is invalid:~%~A")

(defparameter *keyword-missing*
  "The keyword ~A is missing.")

(defparameter *keyword-wrong-type*
  "The value of keyword ~A is not of expected type ~A.")

(defparameter *malformed-config*
  "The configuration is malformed.~%~A")

(defvar *config* nil)

(defmethod config ((option symbol))
  (multiple-value-bind (key value foundp)
      (get-properties *config* (list option))
    (declare (ignore key))
    (if foundp
        (values value t)
        (values nil nil))))

(defmethod (setf config) (new-value (option symbol))
  (let ((new-config (list* option new-value
                           (remove-from-plist *config* option))))
    (validate-config new-config)
    (setf *config* new-config)
    new-value))

(defun default-config-path ()
  "Returns the default pathname to the Gateway configuration file, based
on the value of USER-HOMEDIR-PATHNAME."
  (merge-pathnames (make-pathname :directory '(:relative ".gateway")
                                  :name "config"
                                  :type "sexp")
                   (user-homedir-pathname)))

(defun load-config (&optional config-path)
  "Loads the Gateway configuration file from the provided path, validates it
and sets it as the current Gateway configuration. If no path is provided, the
default path is used.
Returns the new value of *config*."
  (let* ((path (or config-path (default-config-path))))
    (with-input-from-file (stream path :if-does-not-exist :error)
      (let ((config (read stream)))
        (validate-config config)
        (setf *config* config)))))

(defun save-config (&optional config-path)
  "Validates and saves the current Gateway config to the provided path,
overwriting any previous contents. If no path is provided, the default path is
used.
Returns the pathname of the saved config."
  (validate-config *config*)
  (let* ((path (or config-path (default-config-path))))
    (ensure-directories-exist path)
    (with-output-to-file (stream path :element-type 'character
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
      (pprint-plist stream *config*)
      (fresh-line stream)
      path)))

(defun validate-config (config &optional (errorp t))
  "Validates the configuration file by asserting that all mandatory options
are provided and of proper type.

If ERRORP is true, an error is raised if the configuration does not pass
validation. Otherwise, (NIL STRINGS) is returned, where STRINGS is a list of
errors detected in the configuration file."
  (let* ((protocols protest:*protocols*)
         (protocol (find 'config protocols :key #'car))
         (forms (cddr protocol))
         (options (remove-if-not (curry #'eq :option) forms :key #'car))
         (mandatory (remove-if (curry #'eq :optional) options :key #'fifth))
         (list (mapcar (lambda (x) (list (second x) (fourth x))) mandatory))
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
             (error *config-error* (apply #'catn (nreverse errors))))
            (t
             (values nil (nreverse errors)))))))
