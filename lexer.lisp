#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defvar *root* NIL)

(defun peek-char-n (n stream)
  (let ((result (loop repeat n collect (read-char stream))))
    (loop for char in (reverse result)
          repeat n do (unread-char char stream))
    result))

(defun consume (stream)
  (read-char stream))

(defun consume-n (n stream)
  (loop repeat n collect (read-char stream)))

(defun consume-until (matcher stream)
  (loop with output = (make-string-output-stream)
        for (match string) = (multiple-value-list (funcall matcher stream))
        until match
        for char = (read-char stream NIL NIL)
        while char
        do (write-char char output)
        finally (progn
                  (loop for char across string
                        do (unread-char char stream))
                  (return (get-output-stream-string output)))))

(defun matcher-string (string)
  #'(lambda (stream)
      (values
       (let ((read ()))
         (unwind-protect
              (loop for curr across string
                    for curs = (read-char stream NIL NIL) 
                    always curs
                    do (push curs read)
                    always (char= curs curr))
           (loop for char in read
                 do (unread-char char stream))))
       string)))

(defun matcher-or (&rest matchers)
  #'(lambda (stream)
      (loop for matcher in matchers
            for (match string) = (multiple-value-list (funcall matcher stream))
            do (when match
                 (consume-n (length string) stream)
                 (return (values match string)))
            finally (return (values NIL "")))))

(defun matcher-and (&rest matchers)
  #'(lambda (stream)
      (let ((consumed (make-string-output-stream)))
        (loop for matcher in matchers
              for (match string) = (multiple-value-list (funcall matcher stream))
              do (if match
                     (progn
                       (write-string string consumed)
                       (consume-n (length string) stream))
                     (progn
                       (loop for char across (get-output-stream-string consumed)
                             do (unread-char char stream))
                       (return (values NIL ""))))
              finally (return (values T (get-output-stream-string consumed)))))))

(defun matcher-not (matcher)
  #'(lambda (stream)
      (multiple-value-bind (match string) (funcall matcher stream)
        (values (not match) string))))

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

(defun read-name (stream)
  (consume-until (make-matcher (or (is " ") (is "/") (is ">"))) stream))

(defun read-text (stream)
  (make-text-node
   *root*
   (decode-entities
    (consume-until (make-matcher (and (is "<") (not (is " ")))) stream))))

(defun read-comment (stream)
  (loop for char = (read-char stream NIL NIL)
        while char
        do (unless (char= #\- char)
             (unread-char char stream)
             (return)))
  (prog1 (make-comment
          *root*
          (decode-entities
           (consume-until (make-matcher (is "-->")) stream)))
    (consume-n 6 stream)))

(defun read-children (stream)
  (let ((close-tag (format NIL "</~a>" (tag-name *root*))))
    (loop with children = (make-child-array)
          while (peek-char NIL stream NIL NIL)
          for (match string) = (multiple-value-list (funcall (make-matcher (is close-tag)) stream))
          until match
          do (vector-push-extend (or (read-tag stream)
                                     (read-text stream)) children)
          finally (progn (when match
                           (consume-n (length string) stream))
                         (return children)))))

(defun read-attribute-value (stream)
  (decode-entities
   (let ((first (peek-char NIL stream NIL NIL)))
     (if (and first (char= first #\"))
         (prog2 (consume stream)
             (consume-until (make-matcher (is "\"")) stream)
           (consume stream) ;; ??
           (consume stream))
         (consume-until (make-matcher (or (is " ") (is "/") (is ">"))) stream)))))

(defun read-attribute-name (stream)
  (consume-until (make-matcher (or (is "=") (is " ") (is "/") (is ">"))) stream))

(defun read-attribute (stream)
  (let ((name (read-attribute-name stream))
        (next (peek-char NIL stream NIL NIL))
        (value ""))
    (when (and next (char= next #\=))
      (consume stream)
      (setf value (read-attribute-value stream)))
    (cons name value)))

(defun read-attributes (stream)
  (loop with table = (make-attribute-map)
        for char = (peek-char NIL stream NIL NIL)
        do (case char
             ((#\/ #\> NIL)
              (return table))
             (#\Space
              (consume stream))
             (T
              (let ((entry (read-attribute stream)))
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun read-standard-tag (stream name)
  (let ((attrs (read-attributes stream))
        (closing (read-char stream)))
    (case closing
      (#\/ (read-char stream)
       (make-element *root* name :attributes attrs))
      (#\>
       (let ((*root* (make-element *root* name :attributes attrs)))
         (setf (children *root*)
               (read-children stream))
         *root*)))))

(defun read-tag (stream)
  (let ((next-2 (peek-char-n 2 stream)))
    (when (and (char= #\< (first next-2))
               (not (char= #\Space (second next-2))))
      (consume stream)
      (let ((name (read-name stream)))
        (if (and (<= 3 (length name))
                 (string= name "!--" :end1 3))
            (read-comment stream)
            (read-standard-tag stream name))))))

(defun read-root (stream)
  (let ((*root* (make-root)))
    (loop while (peek-char NIL stream NIL NIL)
          do (append-child *root* (or (read-tag stream)
                                      (read-text stream))))
    *root*))
