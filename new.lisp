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
  (:use #:cl
        #:bknr.datastore))

(in-package #:bknr.multistore)

(defvar *store-root* #p"/tmp/gateway/")

(defun make-object-store (directory)
  (make-instance 'mp-store
                 :make-default nil
                 :directory directory
                 :subsystems (list (make-instance 'store-object-subsystem))))

(defclass world ()
  ((name :accessor name
         :initarg :name
         :initform (error "No NAME provided."))
   (maps-store :accessor maps-store)
   (players-store :accessor players-store)
   (chats :accessor chats
          :initform ())))

(defmethod initialize-instance :after ((world world) &key)
  (let* ((directory (merge-pathnames (concatenate 'string (name world) "/") *store-root*))
         (maps-directory (merge-pathnames "maps/" directory))
         (players-directory (merge-pathnames "players/" directory)))
    (ensure-directories-exist directory)
    (setf (maps-store world) (make-object-store maps-directory)
          (players-store world) (make-object-store players-directory))))

(defclass chat ()
  ((name :accessor name
         :initarg :name
         :initform (error "No NAME provided."))
   (world :accessor world
          :initarg :world
          :initform (error "No WORLD provided."))
   (messages-store :accessor messages-store)))

(defmethod initialize-instance :after ((chat chat) &key)
  (let* ((world (world chat))
         (owner-directory (merge-pathnames (concatenate 'string (name world) "/") *store-root*))
         (chats-directory (merge-pathnames "chats/" owner-directory))
         (directory (merge-pathnames (concatenate 'string (name chat) "/") chats-directory)))
    (ensure-directories-exist directory)
    (setf (messages-store chat) (make-object-store directory))
    (push chat (chats world))))

#|
(defparameter *foo-world* (make-instance 'world :name "foo-world"))
(defparameter *quux-world* (make-instance 'world :name "quux-world"))
(defparameter *bar-chat* (make-instance 'chat :name "bar-chat" :world *foo-world*))
(defparameter *baz-chat* (make-instance 'chat :name "baz-chat" :world *foo-world*))
|#

#|

/worlds/foo-world/maps/*
/worlds/foo-world/players/*
/worlds/foo-world/chats/bar-chat/*
/worlds/foo-world/chats/baz-chat/*
/worlds/quux-world/*

|#
