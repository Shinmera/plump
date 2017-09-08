#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(defvar *processing-parsers* (make-hash-table :test 'equalp))

(defun processing-parser (process-name)
  (gethash process-name *processing-parsers*))

(defun (setf processing-parser) (func process-name)
  (setf (gethash process-name *processing-parsers*)
        func))

(defun remove-processing-parser (process-name)
  (remhash process-name *processing-parsers*))

(defmacro define-processing-parser (process-name () &body body)
  `(setf (processing-parser ,(string process-name))
         (lambda () ,@body)))

;; Special handling for processing instructions
(define-tag-dispatcher (process *xml-tags* *html-tags*) (name)
  (and (<= 1 (length name))
       (char= (aref name 0) #\?)))

(define-tag-parser process (name)
  (let* ((name (subseq name 1))
         (text (funcall (or (processing-parser name)
                            (progn (warn "Don't know how to properly parse processing instructions of type ~a!" name)
                                   (processing-parser ""))))))
    (advance-n 2)
    (make-processing-instruction *root* :text text :name (when (string/= "" name) name))))

(define-processing-parser "" ()
  (consume-until (make-matcher (is "?>"))))
