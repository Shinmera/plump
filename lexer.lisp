#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space)))
(defvar *root* NIL)
(defvar *tag-dispatchers* ())
(defvar *string* NIL)
(defvar *length* 0)
(defvar *index* 0)

(defmacro with-lexer-environment ((string) &body body)
  `(let* ((*string* ,string)
          (*length* (length *string*))
          (*index* 0))
     ,@body))

(defmacro define-tag-dispatcher (name (tagvar) test-form &body body)
  (let ((namegens (gensym "NAME")) (posgens (gensym "POSITION")) (valgens (gensym "VALUE")))
    `(let* ((,namegens ',name)
            (,valgens (list ,namegens
                            #'(lambda (,tagvar) ,test-form)
                            #'(lambda (,tagvar) (declare (ignorable ,tagvar)) ,@body)))
            (,posgens (position ,namegens *tag-dispatchers* :key #'first)))
       (if ,posgens
           (setf (nth ,posgens *tag-dispatchers*) ,valgens)
           (push ,valgens *tag-dispatchers*)))))

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

(defun read-name ()
  (consume-until (make-matcher (or (is " ") (is "/>") (is ">")))))

(defun read-text ()
  (make-text-node
   *root*
   (decode-entities
    (consume-until (make-matcher (and (is "<") (not (is " "))))))))

;; Robustify
(defun read-tag-contents ()
  (decode-entities
   (consume-until (make-matcher (or (is "/>") (is ">"))))))

(defun read-children ()
  (let ((close-tag (format NIL "</~a>" (tag-name *root*))))
    (loop with children = (make-child-array)
          while (peek)
          for (match . string) = (funcall (make-matcher (is close-tag)))
          until match
          do (vector-push-extend (or (read-tag) (read-text)) children)
          finally (progn (when match
                           (consume-n (length string)))
                         (return children)))))

(defun read-attribute-value ()
  (decode-entities
   (let ((first (peek)))
     (if (and first (char= first #\"))
         (prog2 (consume)
             (consume-until (make-matcher (is "\"")))
           (consume))
         (consume-until (make-matcher (or (is " ") (is "/>") (is ">"))))))))

(defun read-attribute-name ()
  (consume-until (make-matcher (or (is "=") (is " ") (is "/>") (is ">")))))

(defun read-attribute ()
  (let ((name (read-attribute-name))
        (next (consume))
        (value ""))
    (if (and next (char= next #\=))
        (setf value (read-attribute-value))
        (unread))
    (cons name value)))

(defun read-attributes ()
  (loop with table = (make-attribute-map)
        for char = (peek)
        do (case char
             ((#\/ #\> NIL)
              (return table))
             (#.*whitespace*
              (consume))
             (T
              (let ((entry (read-attribute)))
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun read-standard-tag (name)
  (let* ((closing (consume))
         (attrs (if (char= closing #\Space)
                    (prog1 (read-attributes)
                      (setf closing (consume)))
                    (make-attribute-map))))
    (case closing
      (#\/
       (consume-n 2)
       (make-element *root* name :attributes attrs))
      (#\>
       (let ((*root* (make-element *root* name :attributes attrs)))
         (setf (children *root*)
               (read-children))
         *root*)))))

(defun read-tag ()
  (if (and (char= #\< (consume))
           (not (member (peek) *whitespace* :test #'char=)))     
    (let ((name (read-name)))
      (loop for (d test func) in *tag-dispatchers*
            when (funcall test name)
              do (return (funcall func name))
            finally (return (read-standard-tag name))))
    (progn (unread) NIL)))

(defun read-root (&optional (root (make-root)))
  (let ((*root* root))
    (loop while (peek)
          do (append-child *root* (or (read-tag)
                                      (read-text))))
    *root*))

(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defgeneric parse (input &key root)
  (:documentation "")
  (:method ((input string) &key root)
    (with-lexer-environment (input)
      (if root
          (read-root root)
          (read-root))))
  (:method ((input pathname) &key root)
    (with-open-file (stream input :direction :input)
      (parse stream :root root)))
  (:method ((input stream) &key root)
    (parse (slurp-stream input) :root root)))
