#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defun array-shift (array &key (n 1) (from 0) (to (length array)) (adjust T) (fill NIL f-p))
  "Helper function that shifts a subset of array elements in either direction for a specified amount.
Optionally also extends the array and fills empty space with a given element.

N      --- The amount to be moved. Can either be positive or negative.
FROM   --- Move region start point.
TO     --- Move region end point.
ADJUST --- Whether to adjust the fill pointer and the array bounds.
FILL   --- If provided, empty spaces created by the move will be filled with this element."
  (when (and adjust (array-has-fill-pointer-p array))
    (unless (array-in-bounds-p array (+ (fill-pointer array) n))
      (adjust-array array (+ (fill-pointer array) n)))
    (incf (fill-pointer array) n))
  (if (< 0 n)
      (progn
        (loop repeat (- to from)
              for cursor downfrom (1- to)
              do (setf (aref array (+ cursor n))
                       (aref array cursor)))
        (when f-p
          (loop repeat n
                for cursor from from below (+ to n)
                do (setf (aref array cursor) fill))))
      (progn
        (loop repeat (- to from)
              for cursor from (+ from n)
              do (setf (aref array cursor)
                       (aref array (- cursor n))))
        (when f-p
          (loop repeat (- n)
                for cursor downfrom (1- to) to from
                do (setf (aref array cursor) fill)))))
  array)

(defun vector-push-extend-front (element vector)
  "Pushes the element onto the front of the vector and extends if necessary.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT."
  (array-shift vector)
  (setf (aref vector 0) element)
  (fill-pointer vector))

(defun vector-push-extend-position (element vector position)
  "Pushes the element into the specified position and shifts everything
to the right to make space. This is potentially very costly as all
elements after the given position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-pop-front (vector)
  "Pops the first element off the vector and returns it.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT."
  (let ((front (aref vector 0)))
    (array-shift vector :n -1)
    front))

(defun vector-pop-position (vector position)
  "Pops the element at the given position of the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((el (aref vector position)))
    (array-shift vector :n -1 :from (1+ position))
    el))

(defgeneric vector-append (vector sequence &optional position)
  (:documentation "Appends all elements of the sequence at position of the vector and returns it.
 This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT.")
  (:method ((vector vector) (list list) &optional position)
    (let ((position (or position (length vector))))
      (array-shift vector :n (length list) :from position)
      (loop for i from position
            for item in list
            do (setf (aref vector i) item)))
    vector)
  (:method ((vector vector) (array array) &optional position)
    (let ((position (or position (length vector))))
      (array-shift vector :n (length array) :from position)
      (loop for i from position
            for item across array
            do (setf (aref vector i) item)))
    vector))
