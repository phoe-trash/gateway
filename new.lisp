;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; new.lisp

;; temporarily commented out
;; (in-package #:gateway)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BKNR.MULTISTORE
;;;; © Michał "phoe" Herda 2017
;;;; bknr.multistore.lisp

(defpackage #:bknr.multistore
  (:shadowing-import-from #:closer-mop
                          #:standard-generic-function #:defmethod #:defgeneric
                          #:standard-method #:standard-class)
  (:use #:cl
        #:closer-mop
        #:bknr.datastore
        #:split-sequence))

(in-package #:bknr.multistore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STORES - UTILITY
(defvar *store* 'attempted-to-create-object-without-store-context)
(defvar *store-root* #p"/tmp/gateway/")

(defgeneric store-location (object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-store (store &body body)
    `(let ((*store* ,store))
       ,@body))

  (defmacro with-store-and-transaction ((store &optional label) &body body)
    `(with-store ,store
       (with-transaction (,label)
         ,@body)))

  (defmacro make-stores (object &body stores)
    (let ((definitions (mapcar #'%make-store-definition stores)))
      `(%make-stores ,object ,@definitions)))

  (defun %make-store-definition (symbol)
    (let* ((name (string symbol))
           (store-name (concatenate 'string name "-STORE"))
           (store-path (concatenate 'string name "/")))
      `(,(intern store-name) ,(string-downcase store-path))))

  (defmacro %make-stores (object &body store-definitions)
    (let ((setf-list (%make-stores-list object store-definitions)))
      `(setf ,@setf-list)))

  (defun %make-stores-list (object store-definitions)
    (flet
        ((fn (definition)
           (destructuring-bind (accessor folder) definition
             `((,accessor ,object)
               (make-object-store (merge-pathnames ,folder (store-location ,object)))))))
      (mapcan #'fn store-definitions)))

  (defun make-object-store (directory)
    (ensure-directories-exist directory)
    (make-instance 'mp-store
                   :make-default nil
                   :directory directory
                   :subsystems (list (make-instance 'store-object-subsystem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STORAGE-CLASS
(defclass storage-class (standard-class)
  ())

(defmethod initialize-instance :before ((class storage-class) &key))

(defmethod reinitialize-instance :before ((class storage-class) &key))

(defmethod validate-superclass ((class storage-class) (super standard-class))
  t)

(defun %storage-class-slot-definition (symbol)
  (let* ((slot-name symbol)
         (reader symbol)
         (initarg nil)
         (elements (split-sequence #\- (string-downcase (string symbol))))
         (directory (%ensure-slash (first elements)))
         (initform `(make-object-store ,directory)))
    (assert (= 2 (length elements)))
    (assert (string= "store" (second elements)))
    `(:name ,slot-name
      :readers (,reader)
      :writers ((setf ,reader))
      :initarg ,initarg
      :initform ,initform
      :initfunction #'(lambda () ,initform))))

(defun %ensure-slash (string)
  (if (eql #\/ (elt string (1- (length string))))
      string
      (concatenate 'string string "/")))

;; (defmethod ensure-class-using-class :around (class (name (eql 'bar)) &rest
;;                                              &key direct-slots my-argument)
;;   )

#|
(defclass sample-storage ()
  ()
  (:metaclass storage-class)
  (:stores foo-store bar-store))

(ENSURE-CLASS-USING-CLASS
 #<FOO-CLASS BKNR.MULTISTORE::BAR> BAR
 :METACLASS FOO-CLASS
 :DIRECT-SUPERCLASSES NIL
 :DIRECT-SLOTS ((:INITFUNCTION #<SomeFunction>
                 :NAME MY-SLOT
                 :READERS (BAR-MY-SLOT)
                 :WRITERS ((SETF BAR-MY-SLOT))
                 :INITARGS (:MY-SLOT)
                 :INITFORM NIL))
 :DEFINITION-SOURCE
 #S(SB-C:DEFINITION-SOURCE-LOCATION
    :NAMESTRING "/tmp/new.lisp"
    :TOPLEVEL-FORM-NUMBER 0
    :FORM-NUMBER 0
    :PLIST (:EMACS-BUFFER "new.lisp"
            :EMACS-FILENAME "/tmp/new.lisp"
            :EMACS-PACKAGE "BKNR.MULTISTORE"
            :EMACS-POSITION 2719
            :EMACS-STRING "(defclass bar () ...)"))
 SB-PCL::SAFE-P NIL
 :MY-ARGUMENT (1 2 THREE FOUR OMG)
 :DIRECT-DEFAULT-INITARGS NIL)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WORLD
(defclass world ()
  ((name :accessor name
         :initarg :name
         :initform (error "No NAME provided."))
   (maps-store :accessor maps-store)
   (players-store :accessor players-store)
   (chats :accessor chats
          :initform ()))
  (:metaclass storage-class)
  ;; (:stores maps-store players-store)
  )

(defmethod store-location ((object world))
  (merge-pathnames (concatenate 'string (name object) "/") *store-root*))

(defmethod initialize-instance :after ((world world) &key)
  (make-stores world maps players))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAP
;; needs to be DEFCLASSED inside a transaction
;; (defclass gateway-map (store-object)
;;   ((data :accessor map-data
;;          :initarg :data
;;          :initform ()))
;;   (:metaclass persistent-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CHAT
(defclass chat ()
  ((name :accessor name
         :initarg :name
         :initform (error "No NAME provided."))
   (world :accessor world
          :initarg :world
          :initform (error "No WORLD provided."))
   (messages-store :accessor messages-store)))

(defmethod initialize-instance :after ((chat chat) &key)
  (make-stores chat messages)
  (push chat (chats (world chat))))

(defmethod store-location ((object chat))
  (let* ((owner-directory (store-location (world object)))
         (chats-directory (merge-pathnames "chats/" owner-directory)))
    (merge-pathnames (concatenate 'string (name object) "/") chats-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  data


#|
(defparameter *foo-world* (make-instance 'world :name "foo-world"))
(defparameter *quux-world* (make-instance 'world :name "quux-world"))
(defparameter *bar-chat* (make-instance 'chat :name "bar-chat" :world *foo-world*))
(defparameter *baz-chat* (make-instance 'chat :name "baz-chat" :world *foo-world*))
(let ((*store* (maps-store *foo-world*)))
  (defvar *map-one* (make-instance 'gateway-map :data '(1 2 3 4))))

|#

#|

/worlds/foo-world/maps/*
/worlds/foo-world/players/*
/worlds/foo-world/chats/bar-chat/*
/worlds/foo-world/chats/baz-chat/*
/worlds/quux-world/*

|#
