#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(defvar *all-tag-dispatchers* () "All defined tag dispatchers")
(defvar *tag-dispatchers* () "Active tag dispatcher functions")
(defvar *xml-tags* () "List of XML tag dispatchers")
(defvar *html-tags* () "List of HTML tag dispatchers")

(defstruct tag-dispatcher
  (name (error "NAME required") :type symbol)
  (test (lambda (a) (declare (ignore a))) :type (function (string) boolean))
  (parser (lambda (a) (declare (ignore a))) :type (function (string) boolean))
  (printer (lambda (a) (declare (ignore a))) :type (function (node) boolean)))

(defun tag-dispatcher (name &optional (list *all-tag-dispatchers*))
  "Returns the tag-dispatcher form of NAME from LIST, if any.
A tag dispatcher form is a list made up of NAME, TEST-FUNCTION and DISPATCHER-FUNCTION."
  (find name list :key #'tag-dispatcher-name))

(define-setf-expander tag-dispatcher (name &optional (list '*all-tag-dispatchers*))
  (let ((nameg (gensym "NAME"))
        (disp (gensym "DISP")))
    (values (list nameg)
            (list name)
            (list disp)
            `(progn
               (setf ,list (list* ,disp (remove ,nameg ,list :key #'tag-dispatcher-name)))
               ,disp)
            disp)))

(defun remove-tag-dispatcher (name &optional (list '*all-tag-dispatchers*))
  (setf (symbol-value list) (remove name (symbol-value list) :key #'tag-dispatcher-name)))

(defmacro define-tag-dispatcher ((name &rest lists) (tagvar) &body body)
  "Defines a new tag dispatcher. It is invoked if TEST-FORM passes.

NAME      --- Name to discern the dispatcher with.
LISTS     --- Symbols of lists to which the dispatcher should be added.
TAGVAR    --- Symbol bound to the tag name.
BODY      --- Body forms describing the test to match the tag."
  (let ((test (gensym "TEST"))
        (disp (gensym "DISP")))
    `(let ((,test (lambda (,tagvar) ,@body))
           (,disp (make-tag-dispatcher :name ',name)))
       ,@(loop for list in (list* '*all-tag-dispatchers* lists)
               collect `(let ((,disp (or (tag-dispatcher ',name ,list)
                                         (setf (tag-dispatcher ',name ,list) ,disp))))
                          (setf (tag-dispatcher-test ,disp) ,test))))))

(defmacro define-tag-parser (name (tagvar) &body body)
  "Defines the parser function for a tag dispatcher.

NAME    --- The name of the tag dispatcher. If one with this name 
            cannot be found, an error is signalled.
TAGVAR  --- Symbol bound to the tag name.
BODY    --- Body forms describing the tag parsing behaviour."
  `(setf (tag-dispatcher-parser
          (or (tag-dispatcher ',name)
              (error "No tag dispatcher with name ~s is defined." ',name)))
         (lambda (,tagvar)
           (declare (ignorable ,tagvar))
           ,@body)))

(defmacro define-tag-printer (name (nodevar) &body body)
  "Defines the printer function for a tag dispatcher.

NAME    --- The name of the tag dispatcher. If one with this name 
            cannot be found, an error is signalled.
TAGVAR  --- Symbol bound to the tag name.
BODY    --- Body forms describing the printing behaviour. Write to
            the stream bound to PLUMP-DOM:*STREAM*."
  `(setf (tag-dispatcher-printer
          (or (tag-dispatcher ',name)
              (error "No tag dispatcher with name ~s is defined." ',name)))
         (lambda (,nodevar)
           ,@body)))

(defmacro do-tag-parsers ((test parser &optional result-form) &body body)
  "Iterates over the current *TAG-DISPATCHERS*, binding the TEST and PARSER functions.
Returns RESULT-FORM's evaluated value."
  (let ((disp (gensym "DISPATCHER")))
    `(dolist (,disp *tag-dispatchers* ,result-form)
       (let ((,test (tag-dispatcher-test ,disp))
             (,parser (tag-dispatcher-parser ,disp)))
         ,@body))))

(defmacro do-tag-printers ((test printer &optional result-form) &body body)
  "Iterates over the current *TAG-DISPATCHERS*, binding the TEST and PRINTER functions.
Returns RESULT-FORM's evaluated value."
  (let ((disp (gensym "DISPATCHER")))
    `(dolist (,disp *tag-dispatchers* ,result-form)
       (let ((,test (tag-dispatcher-test ,disp))
             (,printer (tag-dispatcher-printer ,disp)))
         ,@body))))
