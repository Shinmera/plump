#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(defvar *all-tag-dispatchers* ())
(defvar *tag-dispatchers* ())
(defvar *xml-tags* ())
(defvar *html-tags* ())

(defstruct tag-dispatcher
  (name (error "NAME required") :type symbol)
  (test (lambda (a) (declare (ignore a)) NIL) :type (function (string) boolean))
  (parser (lambda (a) (declare (ignore a)) NIL) :type (function (string) (or null node)))
  (printer (lambda (a) (declare (ignore a)) NIL) :type (function (T) boolean)))

(defun tag-dispatcher (name &optional (list *all-tag-dispatchers*))
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
  (let ((test (gensym "TEST"))
        (disp (gensym "DISP")))
    `(let ((,test (lambda (,tagvar) ,@body))
           (,disp (make-tag-dispatcher :name ',name)))
       ,@(loop for list in (list* '*all-tag-dispatchers* lists)
               collect `(let ((,disp (or (tag-dispatcher ',name ,list)
                                         (setf (tag-dispatcher ',name ,list) ,disp))))
                          (setf (tag-dispatcher-test ,disp) ,test))))))

(defmacro define-tag-parser (name (tagvar) &body body)
  `(setf (tag-dispatcher-parser
          (or (tag-dispatcher ',name)
              (error "No tag dispatcher with name ~s is defined." ',name)))
         (lambda (,tagvar)
           (declare (ignorable ,tagvar))
           ,@body)))

(defmacro define-tag-printer (name (nodevar) &body body)
  `(setf (tag-dispatcher-printer
          (or (tag-dispatcher ',name)
              (error "No tag dispatcher with name ~s is defined." ',name)))
         (lambda (,nodevar)
           ,@body)))

(defmacro do-tag-parsers ((test parser &optional result-form) &body body)
  (let ((disp (gensym "DISPATCHER")))
    `(dolist (,disp *tag-dispatchers* ,result-form)
       (let ((,test (tag-dispatcher-test ,disp))
             (,parser (tag-dispatcher-parser ,disp)))
         ,@body))))

(defmacro do-tag-printers ((test printer &optional result-form) &body body)
  (let ((disp (gensym "DISPATCHER")))
    `(dolist (,disp *tag-dispatchers* ,result-form)
       (let ((,test (tag-dispatcher-test ,disp))
             (,printer (tag-dispatcher-printer ,disp)))
         ,@body))))
