#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.lexer)

(defvar *string*)
(defvar *length*)
(defvar *index*)
(setf (documentation '*string* 'variable) "Contains the current string to lex.")
(setf (documentation '*length* 'variable) "Set to the length of the string for bounds checking.")
(setf (documentation '*index* 'variable) "Set to the current reading index.")
(defvar *matchers* (make-hash-table) "Hash table containing matching rules.")
(declaim (fixnum *length* *index*)
         (simple-string *string*)
         (hash-table *matchers*))

(defmacro with-lexer-environment ((string) &body body)
  "Sets up the required lexing environment for the given string."
  `(let* ((*string* ,string)
          (*string* (etypecase *string*
                      (simple-string *string*)
                      (string (copy-seq *string*))))
          (*length* (length *string*))
          (*index* 0))
     (handler-bind ((error #'(lambda (err)
                               (declare (ignore err))
                               (format T "Error during lexing at index ~a~%" *index*))))
       ,@body)))

(declaim (ftype (function () (or character null)) consume)
         (inline consume))
(defun consume ()
  "Consumes a single character if possible and returns it.
Otherwise returns NIL."
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    (prog1 (aref *string* *index*)
      #+plump-debug-lexer (format T "~a +~%" *index*)
      (incf *index*))))

(declaim (ftype (function () (or fixnum null)) advance)
         (inline advance))
(defun advance ()
  "Skips a chracter if possible.
Returns the new index or NIL."
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    #+plump-debug-lexer (format T "~a +~%" *index*)
    (incf *index*)))

(declaim (ftype (function () fixnum) unread)
         (inline unread))
(defun unread ()
  "Steps back a single character if possible.
Returns the new *INDEX*."
  (declare (optimize (speed 3) (safety 0)))
  (when (< 0 *index*)
    #+plump-debug-lexer (format T "~a -~%" *index*)
    (decf *index*))
  *index*)

(declaim (ftype (function () (or character null)) peek)
         (inline peek))
(defun peek ()
  "Returns the next character, if any."
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    #+plump-debug-lexer (format T "~a ?~%" *index*)
    (aref *string* *index*)))

(declaim (ftype (function (fixnum) fixnum) advance-n)
         (inline advance-n))
(defun advance-n (n)
  "Advances by N characters if possible.
Returns the new *INDEX*."
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  #+plump-debug-lexer (format T "~a +~d~%" *index* n)
  (incf *index* n)
  (when (<= *length* *index*)
    (setf *index* *length*))
  *index*)

(declaim (ftype (function (fixnum) fixnum) unread-n)
         (inline unread-n))
(defun unread-n (n)
  "Steps back by N characters if possible.
Returns the new *INDEX*."
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  #+plump-debug-lexer (format T "~a -~d~%" *index* n)
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0))
  *index*)

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

(declaim (ftype (function (list) function) matcher-find))
(defun matcher-find (list)
  "Creates a matcher function that returns T if the character is found in the given list."
  #'(lambda ()
      (let ((char (peek)))
        (and char (member char list :test #'char=)))))

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
	(when (< *index* *length*))
        (funcall matcher))))

(declaim (ftype (function (function) function) matcher-prev))
(defun matcher-prev (matcher)
  "Creates a matcher environment that peeks behind."
  #'(lambda ()
      (let ((*index* (1- *index*)))
	(when (<= 0 *index*)
	  (funcall matcher)))))

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
                 (case (find-symbol (string (car form)) "PLUMP-LEXER")
                   (not 'matcher-not)
                   (and 'matcher-and)
                   (or 'matcher-or)
                   (is (typecase (second form)
                         (string 'matcher-string)
                         (character 'matcher-character)
                         (T 'matcher-string)))
                   (in 'matcher-range)
                   (next 'matcher-next)
                   (prev 'matcher-prev)
                   (any 'matcher-any)
                   (find 'matcher-find)
                   (T (car form)))
                 (mapcar #'transform (cdr form)))))))
    (transform form)))

(defmacro define-matcher (name form)
  "Associates NAME as a keyword to the matcher form. You can then use the keyword in further matcher rules."
  `(setf (gethash ,(intern (string name) "KEYWORD") *matchers*) (make-matcher ,form)))
