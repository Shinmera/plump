#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(defvar *tag-dispatchers* () "Active tag dispatcher functions")
(defvar *xml-tags* () "List of XML tag dispatchers")
(defvar *html-tags* () "List of HTML tag dispatchers")

(defun tag-dispatcher (name &optional (list *tag-dispatchers*))
  "Returns the tag-dispatcher form of NAME from LIST, if any.
A tag dispatcher form is a list made up of NAME, TEST-FUNCTION and DISPATCHER-FUNCTION."
  (find name list :key #'first))

(defmacro set-tag-dispatcher (name test-func dispatch-func &optional (list '*tag-dispatchers*))
  "Adds or updates the DISPATCHER-FORM for NAME on LIST."
  (let ((position (gensym "POSITION"))
        (form (gensym "FORM")))
    `(let* ((,position (position ,name ,list :key #'first))
            (,form (list ,name ,test-func ,dispatch-func)))
       (if ,position
           (setf (nth ,position ,list) ,form)
           (push ,form ,list)))))

(defmacro define-tag-dispatcher ((name &rest lists) (tagvar) test-form &body body)
  "Defines a new tag dispatcher. It is invoked if TEST-FORM passes.

NAME      --- Name to discern the dispatcher with.
LISTS     --- Symbols of lists to which the dispatcher should be added.
TAGVAR    --- Symbol bound to the tag name.
TEST-FORM --- Form that should return non-NIL if the dispatcher should
              be invoked.
BODY      ::= form*"
  (let ((test (gensym "TEST"))
        (disp (gensym "DISP")))
    `(let ((,test #'(lambda (,tagvar) ,test-form))
           (,disp #'(lambda (,tagvar) (declare (ignorable name)) ,@body)))
       ,@(loop for list in lists
               collect `(set-tag-dispatcher ',name ,test ,disp ,list)))))
(indent:define-indentation define-tag-dispatcher (4 4 6 &body))

(defun remove-tag-dispatcher (name)
  "Removes the tag dispatcher of NAME."
  (setf *tag-dispatchers*
        (delete name *tag-dispatchers* :key #'car)))

(defmacro do-tag-dispatchers ((test dispatcher &optional result-form) &body body)
  "Iterates over the current *TAG-DISPATCHERS*, binding the TEST and DISPATCHER functions.
Returns RESULT-FORM's evaluated value."
  (let ((disp (gensym "DISPATCHER"))
        (name (gensym "NAME")))
    `(loop for ,disp in *tag-dispatchers*
           do (destructuring-bind (,name ,test ,dispatcher) ,disp
                (declare (ignore ,name))
                ,@body)
           finally (return ,result-form))))
