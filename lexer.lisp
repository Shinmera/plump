#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)
(defvar *string* NIL)
(defvar *length* 0)
(defvar *index* 0)

(defmacro with-lexer-environment ((string) &body body)
  `(let* ((*string* ,string)
          (*length* (length *string*))
          (*index* 0))
     ,@body))

;; Fix to use smarter scheme that doesn't rely on a check every time.
(defun unread ()
  (format T "-")
  (when (< 0 *index*)
    (decf *index*)))

(defun peek ()
  (format T "?")
  (when (< *index* *length*)
    (elt *string* *index*)))

(defun consume ()
  (format T "+")
  (when (< *index* *length*)
    (prog1 (elt *string* *index*)
      (incf *index*))))

(defun peek-n (n)
  (loop for i from *index*
        while (< i *length*)
        repeat n
        do (format T "?")
        collect (elt *string* i)))

(defun consume-n (n)
  (format T "~a" (make-string n :initial-element #\+))
  (incf *index* n)
  (when (<= *length* *index*)
    (setf *index* (1- *length*))))

(defun unread-n (n)
  (format T "~a" (make-string n :initial-element #\-))
  (decf *index* n)
  (when (< *index* 0)
    (setf *index* 0)))

(defun consume-until (matcher) 
  (loop with output = (make-string-output-stream)
        for (match . string) = (funcall matcher)
        until match
        for char = (consume)
        while char
        do (write-char char output)
        finally (return (get-output-stream-string output))))

(defun matcher-string (string)
  #'(lambda ()
      (cons
       (let ((read 0))
         (unwind-protect
              (loop for curr across string
                    for curs = (consume) 
                    always curs
                    do (incf read)
                    always (char= curs curr))
           (unread-n read)))
       string)))

(defun matcher-or (&rest matchers)
  #'(lambda () 
      (loop for matcher in matchers
            for (match . string) = (funcall matcher)
            do (when match
                 (return (cons match string)))
            finally (return (cons NIL "")))))

(defun matcher-and (&rest matchers)
  #'(lambda ()
      (let ((consumed (make-string-output-stream)))
        (loop for matcher in matchers
              for (match . string) = (funcall matcher)
              do (if match
                     (progn
                       (consume-n (length string))
                       (write-string string consumed))
                     (progn
                       (unread-n (length (get-output-stream-string consumed)))
                       (return (cons NIL ""))))
              finally (let ((consumed (get-output-stream-string consumed)))
                        (unread-n (length consumed))
                        (return (cons T consumed)))))))

(defun matcher-not (matcher)
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
