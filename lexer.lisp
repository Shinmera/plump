#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)
(defvar *string* "Contains the current string to lex.")
(defvar *length* 0 "Set to the length of the string for bounds checking.")
(defvar *index* 0 "Set to the current reading index.")
(declaim (fixnum *length* *index*)
         (simple-string *string*))

(defmacro with-lexer-environment ((string) &body body)
  "Sets up the required lexing environment for the given string."
  `(let* ((*string* ,string)
          (*length* (length *string*))
          (*index* 0))
     ,@body))

(defun consume ()
  "Consumes a single character if possible and returns it.
Otherwise returns NIL."
  (when (< *index* *length*)
    (prog1 (elt *string* *index*)
      (incf *index*))))

(defun unread ()
  "Steps back a single character if possible.
Returns the new *INDEX*."
  (when (< 0 *index*)
    (decf *index*))
  *index*)

(defun peek ()
  "Returns the next character, if any."
  (when (< *index* *length*)
    (elt *string* *index*)))

(defun consume-n (n)
  "Advances by N characters if possible.
Returns the new *INDEX*."
  (declare (fixnum n))
  (incf *index* n)
  (when (<= *length* *index*)
    (setf *index* (1- *length*)))
  *index*)

(defun unread-n (n)
  "Steps back by N characters if possible.
Returns the new *INDEX*."
  (declare (fixnum n))
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0))
  *index*)

(defun peek-n (n)
  "Returns the next N characters as a list.
This list is cut short if it is not possible to
peek ahead farther."
  (declare (fixnum n))
  (loop for i from *index*
        while (< i *length*)
        repeat n
        collect (elt *string* i)))

(declaim (ftype (function (function) string) consume-until))
(defun consume-until (matcher)
  "Consumes until the provided matcher function returns positively.
Returns the substring that was consumed."
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
  "Creates a matcher function that attempts to match the given string."
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
  "Creates a matcher function that returns successfully if any of the
sub-expressions return successfully. The first match is returned, if any."
  #'(lambda () 
      (loop for matcher of-type function in matchers
            for (match . string) = (funcall matcher)
            do (when match
                 (return (cons (the boolean match) (the simple-string string))))
            finally (return (cons NIL "")))))

(declaim (ftype (function (&rest function) function) matcher-and))
(defun matcher-and (&rest matchers)
  "Creates a matcher function that returns if all of the sub-expressions
return successfully. The last match is returned, if all."
  #'(lambda ()
      (loop for matcher of-type function in matchers
            for (match . string) = (funcall matcher)
            do (unless match
                   (return (cons NIL "")))
            finally (return (cons T string)))))

(declaim (ftype (function (function) function) matcher-not))
(defun matcher-not (matcher)
  "Creates a matcher function that inverts the result of the sub-expression."
  (declare (function matcher))
  #'(lambda ()
       (let ((result (funcall matcher)))
        (cons (not (car result)) (cdr result)))))

(defmacro make-matcher (form)
  "Macro to create a matcher chain."
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
