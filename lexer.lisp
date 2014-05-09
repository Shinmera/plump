#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)
(defvar *string* "Contains the current string to lex.")
(defvar *length* 0 "Set to the length of the string for bounds checking.")
(defvar *index* 0 "Set to the current reading index.")
(defvar *matchers* (make-hash-table) "Hash table containing matching rules.")
(declaim (fixnum *length* *index*)
         (string *string*))

(defmacro with-lexer-environment ((string) &body body)
  "Sets up the required lexing environment for the given string."
  `(let* ((*string* ,string)
          (*length* (length *string*))
          (*index* 0))
     (handler-bind ((error #'(lambda (err)
                               (declare (ignore err))
                               (format T "Error during lexing at index ~a~%" *index*))))
       ,@body)))

(defun consume ()
  "Consumes a single character if possible and returns it.
Otherwise returns NIL."
  (when (< *index* *length*)
    (prog1 (elt *string* *index*)
      ;;(format T "~a +~%" *index*)
      (incf *index*))))

(defun advance ()
  "Skips a chracter if possible.
Returns the new index or NIL."
  (when (< *index* *length*)
    ;;(format T "~a +~%" *index*)
    (incf *index*)))

(defun unread ()
  "Steps back a single character if possible.
Returns the new *INDEX*."
  (when (< 0 *index*)
    ;;(format T "~a -~%" *index*)
    (decf *index*))
  *index*)

(defun peek ()
  "Returns the next character, if any."
  (when (< *index* *length*)
    ;;(format T "~a ?~%" *index*)
    (elt *string* *index*)))

(defun advance-n (n)
  "Advances by N characters if possible.
Returns the new *INDEX*."
  (declare (fixnum n))
  ;;(format T "~a +~d~%" *index* n)
  (incf *index* n)
  (when (<= *length* *index*)
    (setf *index* *length*))
  *index*)

(defun unread-n (n)
  "Steps back by N characters if possible.
Returns the new *INDEX*."
  (declare (fixnum n))
  ;;(format T "~a -~d~%" *index* n)
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0))
  *index*)

(defun peek-n (n)
  "Returns the next N characters as a list.
This list is cut short if it is not possible to
peek ahead farther."
  (declare (fixnum n))
  ;;(format T "~a ?~d~%" *index* n)
  (loop for i from *index*
        while (< i *length*)
        repeat n
        collect (elt *string* i)))

(declaim (ftype (function (function) string) consume-until))
(defun consume-until (matcher)
  "Consumes until the provided matcher function returns positively.
Returns the substring that was consumed."
  (declare (function matcher))
  (loop with start = *index*
        until (funcall matcher)
        while (advance)
        finally (return (subseq *string* start *index*))))

(declaim (ftype (function (character) function) matcher-character))
(defun matcher-character (character)
  "Creates a matcher function that attempts to match the given character."
  #'(lambda ()
      (let ((char (peek)))
        (when char
          (char= char character)))))

(declaim (ftype (function (simple-string) function) matcher-string))
(defun matcher-string (string)
  "Creates a matcher function that attempts to match the given string."
  (declare (simple-string string))
  (let ((len (length string)))
    #'(lambda ()
        (let ((len (+ *index* len)))
          (and (<= len *length*)
               (string= string *string* :start2 *index* :end2 len))))))

(declaim (ftype (function ((or fixnum character string) (or fixnum character string)) function) matcher-range))
(defun matcher-range (from to)
  "Creates a matcher that checks a range according to the next character's CHAR-CODE."
  (flet ((normalize (in) (etypecase in
                           (fixnum in)
                           (character (char-code in))
                           (string (char-code (aref in 0))))))
    (let ((from (normalize from))
          (to (normalize to)))
      #'(lambda ()
          (let ((char (peek)))
            (when char
              (<= from (char-code char) to)))))))

(declaim (ftype (function (&rest function) function) matcher-or))
(defun matcher-or (&rest matchers)
  "Creates a matcher function that returns successfully if any of the
sub-expressions return successfully. The first match is returned, if any."
  #'(lambda ()
      (loop for matcher of-type function in matchers
            thereis (funcall matcher))))

(declaim (ftype (function (&rest function) function) matcher-and))
(defun matcher-and (&rest matchers)
  "Creates a matcher function that returns if all of the sub-expressions
return successfully. The last match is returned, if all."
  #'(lambda ()
      (loop for matcher of-type function in matchers
            always (funcall matcher))))

(declaim (ftype (function (function) function) matcher-not))
(defun matcher-not (matcher)
  "Creates a matcher function that inverts the result of the sub-expression."
  (declare (function matcher))
  #'(lambda ()
      (not (funcall matcher))))

(declaim (ftype (function (function) function) matcher-next))
(defun matcher-next (matcher)
  "Creates a matcher environment that peeks ahead one farther."
  #'(lambda ()
      (let ((*index* (1+ *index*)))
        (funcall matcher))))

(defmacro matcher-any (&rest is)
  "Shorthand for (or (is a) (is b)..)"
  `(matcher-or ,@(loop for i in is
                       collect `(,(typecase i
                                    (string 'matcher-string)
                                    (character 'matcher-character)
                                    (T 'matcher-string)) ,i))))

(defmacro make-matcher (form)
  "Macro to create a matcher chain."
  (labels ((transform (form)
             (etypecase form
               (keyword
                `(gethash ',form *matchers*))
               (atom form)
               (T
                (cons
                 (case (find-symbol (string (car form)) "PLUMP")
                   (not 'matcher-not)
                   (and 'matcher-and)
                   (or 'matcher-or)
                   (is (typecase (second form)
                         (string 'matcher-string)
                         (character 'matcher-character)
                         (T 'matcher-string)))
                   (in 'matcher-range)
                   (next 'matcher-next)
                   (any 'matcher-any)
                   (T (car form)))
                 (mapcar #'transform (cdr form)))))))
    (transform form)))

(defmacro define-matcher (name form)
  `(setf (gethash ,(intern (string name) "KEYWORD") *matchers*) (make-matcher ,form)))
