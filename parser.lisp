#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space)))
(defvar *root*)
(defvar *tag-dispatchers* ())

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

(defun read-name ()
  (consume-until (make-matcher (or (is " ") (is "/>") (is ">")))))

(defun read-text ()
  (make-text-node
   *root*
   (decode-entities
    (consume-until (make-matcher (and (is "<") (not (is " "))))))))

;; Robustify against strings inside containing >
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
                           (consume-n (length (the simple-string string))))
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
              when (funcall (the function test) name)
                do (return (funcall (the function func) name))
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
