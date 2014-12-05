#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(defvar *processing-parsers* (make-hash-table :test 'equalp))

(defun processing-parser (process-name)
  "Return the processing-parser function for PROCESS-NAME. SETF-able."
  (gethash process-name *processing-parsers*))

(defun (setf processing-parser) (func process-name)
  "Set the processing-parser function for PROCESS-NAME."
  (setf (gethash process-name *processing-parsers*)
        func))

(defun remove-processing-parser (process-name)
  "Remove the processing-parser for PROCESS-NAME."
  (remhash process-name *processing-parsers*))

(defmacro define-processing-parser (process-name () &body body)
  "Defines a new processing-instruction parser. 
It is invoked if a processing-instruction (<?) with PROCESS-NAME is encountered.
The lexer will be at the point straight after reading in the PROCESS-NAME.
Expected return value is a string to use as the processing-instructions' TEXT.
The closing tag (?>) should NOT be consumed by a processing-parser."
  `(setf (processing-parser ,(string process-name))
         #'(lambda () ,@body)))

;; Special handling for processing instructions
(define-tag-dispatcher (process *xml-tags* *html-tags*) (name)
      (and (<= 1 (length name))
           (char= (aref name 0) #\?))
  (let* ((name (subseq name 1))
         (text (funcall (or (processing-parser name)
                            (progn (warn "Don't know how to properly parse processing instructions of type ~a!" name)
                                   (processing-parser ""))))))
    (advance-n 2)
    (make-processing-instruction *root* :text text :name (when (string/= "" name) name))))

(define-processing-parser "" ()
  (consume-until (make-matcher (is "?>"))))
