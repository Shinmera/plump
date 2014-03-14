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
        until (funcall matcher)
        for char = (consume)
        while char
        do (write-char char output)
        finally (return (get-output-stream-string output))))

;; 2.0
;; Each matcher can return three values: NIL :FAIL :PASS.
;; NIL indicates that the matcher is not complete yet and
;; needs more tokens to be sure. When the matcher is done,
;; he continues to return his result for any subsequent
;; index.

(defun matcher-is (i string)
  (let ((gens (gensym "STRING")))
    (etypecase string
      (symbol
       `(let ((,gens ,string))
          (when (<= (length ,gens) ,i)
            (if (string= ,gens *string* :start2 *index* :end2 (+ *index* (length ,gens)))
                :PASS :FAIL))))
      (string
       `(when (<= ,(length string) ,i)
          (if (string= ,string *string* :start2 *index* :end2 (+ *index* ,(length string)))
              :PASS :FAIL))))))

(defun matcher-or (forms)
  (let ((results (gensym "RESULTS")) (result (gensym "RESULT")))
    `(let ((,results (list ,@forms)))
       (loop for ,result in ,results
             if (eq ,result :PASS)
               do (return :PASS)
             if (not ,result)
               do (return NIL)
             finally (return :FAIL)))))

(defun matcher-and (forms)
  (let ((results (gensym "RESULTS")) (result (gensym "RESULT")))
    `(let ((,results (list ,@forms)))
       (loop for ,result in ,results
             if (eq ,result :FAIL)
               do (return :FAIL)
             if (not ,result)
               do (return NIL)
             finally (return :PASS)))))

(defun matcher-not (form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when ,result
         (if (eq ,result :PASS) :FAIL :PASS)))))

(defmacro make-matcher (form)
  (let ((i (gensym "I")) (j (gensym "J")))
    (labels ((expand (form)
               (etypecase form
                 (atom form)
                 (cons (let ((subforms (mapcar #'expand (cdr form))))
                         (case (car form)
                           (is (matcher-is i (car subforms)))
                           (or (matcher-or subforms))
                           (and (matcher-and subforms))
                           (not (matcher-not (car subforms)))
                           (T (cons (car form) subforms))))))))
      `#'(lambda ()
           (loop for ,i from 0
                 for ,j from *index* below *length*
                 for result = ,(expand form)
                 until result
                 finally (return (eq result :PASS)))))))
