;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016
;;;; gateway.lisp

(in-package #:gateway)

(defclass world-map ()
  ((%array :initform nil
	   :initarg :array
	   :accessor array-of
	   :type (array t 2))))

(defprint world-map
  (print-unreadable-object (obj stream :type t)
    (format stream "~{(~D ~D)~}~%" (dimensions obj))
    (princ (print-world-map obj) stream)))

(defun print-world-map (world-map) 
  (let ((x-dimension (x-dimension world-map))
	(y-dimension (y-dimension world-map))) 
    (labels ((item (world-map x y)
	       (if (and (< -1 x x-dimension) (< -1 y y-dimension))
		   (object-at world-map x y)
		   :void))
	     (print-world (result)
	       (with-output-to-string (stream)
		 (iter (for row in result)
		   (iter (for element in row)
		     (princ (%get-type element) stream))
		   (terpri stream)))))
      (print-world (iter (for y from -1 to y-dimension)
		     (collect (iter (for x from -1 to x-dimension)
				(collect (item world-map x y)))))))))

(defun %get-type (element)
  (cond ((eq element :void)
	 #\#)
	((null element)
	 #\.)
	((typep element 'persona)
	 (aref (name-of element) 0))
	(t
	 #\?)))

(defconstructor (world-map dimensions)
  (cond ((and (null (array-of world-map)) (null dimensions))
	 (error "Must provide either :ARRAY or :DIMENSIONS."))
	((and (array-of world-map) dimensions)
	 (error "Cannot provide both :ARRAY and :DIMENSIONS."))
	(dimensions
	 (check-type dimensions list)
	 (check-type (first dimensions) integer)
	 (check-type (second dimensions) integer)
	 (let ((array (make-array dimensions :initial-element nil)))
	   (setf (array-of world-map) array)))))

(defmethod dimensions ((world-map world-map))
  (array-dimensions (array-of world-map)))

(defmethod x-dimension ((world-map world-map))
  (array-dimension (array-of world-map) 0))

(defmethod y-dimension ((world-map world-map))
  (array-dimension (array-of world-map) 1))

(defmethod object-at ((world-map world-map) (x integer) (y integer))
  (let* ((array (array-of world-map))
	 (x-size (array-dimension array 0))
	 (y-size (array-dimension array 1)))
    (cond ((or (< x 0) (< x-size x))
	   (error "X must be between 0 and ~D." (1- x-size)))
	  ((or (< y 0) (< y-size y))
	   (error "Y must be between 0 and ~D." (1- y-size)))
	  (t
	   (aref array x y)))))

(defmethod resize ((world-map world-map) up left down right &key initial-element)
  (resize-array (array-of world-map) up left down right :initial-element initial-element)
  world-map)

(defun resize-array (array up left down right &key initial-element) 
  (flet ((res-x (array) (resize-x array up down	:initial-element initial-element))
	 (res-y (array) (resize-y array left right :initial-element initial-element)))
    (res-y (res-x array))))

(defun offset-array (array x-offset y-offset &key initial-element)
  (check-type x-offset integer)
  (check-type y-offset integer)
  (let ((new-array (make-array (array-dimensions array) :initial-element initial-element)))
    (iter (for x from 0 to (array-dimension array 0))
      (iter (for y from 0 to (array-dimension array 1))
	(ignore-errors ;; kids, don't do this at home
	 (setf (aref new-array x y)
	       (aref array (- x x-offset) (- y y-offset))))))
    new-array))

(defun resize-x (array up down &key initial-element) 
  (check-type up integer)
  (check-type down integer)
  (let* ((x (array-dimension array 0))
	 (y (array-dimension array 1))
	 (new-x (+ x down up)) 
	 (init initial-element)) 
    (flet ((adjust (array) (adjust-array array (list new-x y) :initial-element init))
	   (offset (array) (offset-array array up 0 :initial-element init)))
      (if (plusp up)
	  (offset (adjust array))
	  (adjust (offset array))))))

(defun resize-y (array left right &key initial-element)
  (check-type left integer)
  (check-type right integer)
  (let* ((x (array-dimension array 0))
	 (y (array-dimension array 1))
	 (new-y (+ y left right))
	 (init initial-element)) 
    (flet ((adjust (array) (adjust-array array (list x new-y) :initial-element init))
	   (offset (array) (offset-array array 0 left :initial-element init)))
      (if (plusp left)
	  (offset (adjust array))
	  (adjust (offset array))))))
