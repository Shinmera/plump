#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)
(defvar *string*)
(defvar *length* 0)
(defvar *index* 0)
(declaim (fixnum *length* *index*)
         (simple-string *string*))

(defmacro with-lexer-environment ((string) &body body)
  `(let* ((*string* ,string)
          (*length* (length *string*))
          (*index* 0))
     ,@body))

(defun consume ()
  (when (< *index* *length*)
    (prog1 (elt *string* *index*)
      (incf *index*))))

(defun unread ()
  (when (< 0 *index*)
    (decf *index*)))

(defun peek ()
  (when (< *index* *length*)
    (elt *string* *index*)))

(defun consume-n (n)
  (declare (fixnum n))
  (incf *index* n)
  (when (<= *length* *index*)
    (setf *index* (1- *length*))))

(defun unread-n (n)
  (declare (fixnum n))
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0)))

(defun peek-n (n)
  (declare (fixnum n))
  (loop for i from *index*
        while (< i *length*)
        repeat n
        collect (elt *string* i)))

(declaim (ftype (function (function) string) consume-until))
(defun consume-until (matcher)
  (declare (function matcher))
  (loop with output = (make-string-output-stream)
        for (match . string) = (funcall matcher)
        until match
        for char = (consume)
        while char
        do (write-char char output)
        finally (return (get-output-stream-string output))))

(declaim (ftype (function (simple-string) function) matcher-string))
(defun matcher-string (string)
  (declare (simple-string string))
  #'(lambda ()
      (cons
       (the boolean
            (let ((read 0))
              (declare (fixnum read))
              (unwind-protect
                   (loop for curr across string
                         for curs = (consume) 
                         always curs
                         do (incf read)
                         always (char= curs curr))
                (unread-n read))))
       (the simple-string string))))

(declaim (ftype (function (&rest function) function) matcher-or))
(defun matcher-or (&rest matchers)
  #'(lambda () 
      (loop for matcher of-type function in matchers
            for (match . string) = (funcall matcher)
            do (when match
                 (return (cons (the boolean match) (the simple-string string))))
            finally (return (cons NIL "")))))

(declaim (ftype (function (&rest function) function) matcher-and))
(defun matcher-and (&rest matchers)
  #'(lambda ()
      (let ((consumed (make-string-output-stream)))
        (loop for matcher of-type function in matchers
              for (match . string) = (funcall matcher)
              do (if match
                     (progn
                       (consume-n (length (the simple-string string)))
                       (write-string string consumed))
                     (progn
                       (unread-n (length (get-output-stream-string consumed)))
                       (return (cons NIL ""))))
              finally (let ((consumed (get-output-stream-string consumed)))
                        (unread-n (length consumed))
                        (return (cons T consumed)))))))

(declaim (ftype (function (function) function) matcher-not))
(defun matcher-not (matcher)
  (declare (function matcher))
  #'(lambda ()
       (let ((result (funcall matcher)))
        (cons (not (car result)) (cdr result)))))

(defmacro make-matcher (form)
  (labels ((transform (form)
             (etypecase form
               (atom form)
               (T
                (cons (case (car form)
                        (not 'matcher-not)
                        (and 'matcher-and)
                        (or 'matcher-or)
                        (is 'matcher-string)
                        (T (car form)))
                      (mapcar #'transform (cdr form)))))))
    (transform form)))
