#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.lexer)

(defvar *string*)
(defvar *length*)
(defvar *index*)
(defvar *matchers* (make-hash-table))
(declaim (fixnum *length* *index*)
         (simple-string *string*)
         (hash-table *matchers*))

(defmacro with-lexer-environment ((string) &body body)
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
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    (prog1 (aref *string* *index*)
      #+plump-debug-lexer (format T "~a +~%" *index*)
      (incf *index*))))

(declaim (ftype (function () (or fixnum null)) advance)
         (inline advance))
(defun advance ()
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    #+plump-debug-lexer (format T "~a +~%" *index*)
    (incf *index*)))

(declaim (ftype (function () fixnum) unread)
         (inline unread))
(defun unread ()
  (declare (optimize (speed 3) (safety 0)))
  (when (< 0 *index*)
    #+plump-debug-lexer (format T "~a -~%" *index*)
    (decf *index*))
  *index*)

(declaim (ftype (function () (or character null)) peek)
         (inline peek))
(defun peek ()
  (declare (optimize (speed 3) (safety 0)))
  (when (< *index* *length*)
    #+plump-debug-lexer (format T "~a ?~%" *index*)
    (aref *string* *index*)))

(declaim (ftype (function (fixnum) fixnum) advance-n)
         (inline advance-n))
(defun advance-n (n)
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
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  #+plump-debug-lexer (format T "~a -~d~%" *index* n)
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0))
  *index*)

(declaim (ftype (function (function) string) consume-until))
(defun consume-until (matcher)
  (declare (function matcher))
  (loop with start = *index*
        until (funcall matcher)
        while (advance)
        finally (return (subseq *string* start *index*))))

(declaim (ftype (function (character) function) matcher-character))
(defun matcher-character (character)
  #'(lambda ()
      (let ((char (peek)))
        (when char
          (char= char character)))))

(declaim (ftype (function (simple-string) function) matcher-string))
(defun matcher-string (string)
  (declare (simple-string string))
  (let ((len (length string)))
    #'(lambda ()
        (let ((len (+ *index* len)))
          (and (<= len *length*)
               (string= string *string* :start2 *index* :end2 len))))))

(declaim (ftype (function ((or fixnum character string) (or fixnum character string)) function) matcher-range))
(defun matcher-range (from to)
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
  #'(lambda ()
      (let ((char (peek)))
        (and char (member char list :test #'char=)))))

(declaim (ftype (function (&rest function) function) matcher-or))
(defun matcher-or (&rest matchers)
  #'(lambda ()
      (loop for matcher of-type function in matchers
            thereis (funcall matcher))))

(declaim (ftype (function (&rest function) function) matcher-and))
(defun matcher-and (&rest matchers)
  #'(lambda ()
      (loop for matcher of-type function in matchers
            always (funcall matcher))))

(declaim (ftype (function (function) function) matcher-not))
(defun matcher-not (matcher)
  (declare (function matcher))
  #'(lambda ()
      (not (funcall matcher))))

(declaim (ftype (function (function) function) matcher-next))
(defun matcher-next (matcher)
  #'(lambda ()
      (let ((*index* (1+ *index*)))
	(when (< *index* *length*))
        (funcall matcher))))

(declaim (ftype (function (function) function) matcher-prev))
(defun matcher-prev (matcher)
  #'(lambda ()
      (let ((*index* (1- *index*)))
	(when (<= 0 *index*)
	  (funcall matcher)))))

(defmacro matcher-any (&rest is)
  `(matcher-or ,@(loop for i in is
                       collect `(,(typecase i
                                    (string 'matcher-string)
                                    (character 'matcher-character)
                                    (T 'matcher-string)) ,i))))

(defmacro make-matcher (form)
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
  `(setf (gethash ,(intern (string name) "KEYWORD") *matchers*) (make-matcher ,form)))
