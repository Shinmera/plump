#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defun array-shift (array &key (n 1) (from 0) (to (length array)) (adjust T) (fill NIL f-p))
  (when (and (array-has-fill-pointer-p array)
             adjust)
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
                for cursor from from below to
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
  (array-shift vector)
  (setf (aref vector 0) element)
  (fill-pointer vector))

(defun vector-push-extend-position (element vector position)
  (array-shift vector :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-pop-front (vector)
  (let ((front (aref vector 0)))
    (array-shift vector :n -1)
    front))

(defun vector-pop-position (vector position)
  (let ((el (aref vector position)))
    (array-shift vector :n -1 :from (1+ position))
    el))
