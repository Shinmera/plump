#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page)
    "List containing all whitespace characters."))
(defvar *root*)
(setf (documentation '*root* 'variable) "Object containing the current node to set as parent.")

(define-matcher whitespace (find *whitespace*))
(define-matcher name (or (in #\a #\z) (in #\? #\Z) (in #\- #\:) (any #\\ #\_ #\! #\# #\[ #\])))
(define-matcher tag-end (or (and (is #\/) (next (is #\>))) (is #\>)))

(defun read-name ()
  "Reads and returns a tag name."
  (consume-until (make-matcher (or (not :name) :tag-end))))

(defun read-text ()
  "Reads and returns a text-node."
  (make-text-node
   *root*
   (decode-entities
    (consume-until (make-matcher (and (is #\<) (next :name)))))))

;; Robustify against strings inside containing >
(defun read-tag-contents ()
  "Reads and returns all tag contents. 
E.g. <foo bar baz> => bar baz"
  (decode-entities
   (consume-until (make-matcher :tag-end))))

(defun read-children ()
  "Read all children of the current *root* until the closing tag for *root* is encountered."
  (let ((close-tag (concatenate 'string "</" (tag-name *root*))))
    (loop while (peek)
          for match = (funcall (make-matcher (is close-tag)))
          until match
          do (or (read-tag) (read-text))
          finally (when match
                    (loop for char = (consume)
                          until (or (not char) (char= char #\>)))))))

(defun read-attribute-value ()
  "Reads an attribute value, either enclosed in quotation marks or until a space or tag end."
  (decode-entities
   (let ((first (peek)))
     (case first
       (#\" (prog2 (advance)
                (consume-until (make-matcher (is #\")))
              (advance)))
       (#\' (prog2 (advance)
                (consume-until (make-matcher (is #\')))
              (advance)))
       (T (consume-until (make-matcher (or :whitespace :tag-end))))))))

(defun read-attribute-name ()
  "Reads an attribute name."
  (consume-until (make-matcher (or (is #\=) :whitespace :tag-end))))

(defun read-attribute ()
  "Reads an attribute and returns it as a key value cons."
  (let ((name (read-attribute-name))
        (next (consume))
        (value ""))
    (cond
      ((and next (char= next #\=))
       (setf value (read-attribute-value)))
      ((not next)
       (cons name NIL))
      (T
       (unread)))
    (cons name value)))

(defun read-attributes ()
  "Reads as many attributes as possible from a tag and returns them as an attribute map."
  (loop with table = (make-attribute-map)
        for char = (peek)
        do (case char
             ((#\/ #\> NIL)
              (return table))
             (#.*whitespace*
              (advance))
             (T
              (let ((entry (read-attribute)))
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun read-standard-tag (name)
  "Reads an arbitrary tag and returns it.
This recurses with READ-CHILDREN."
  (let* ((closing (consume))
         (attrs (if (member closing *whitespace* :test #'eql)
                    (prog1 (read-attributes)
                      (setf closing (consume)))
                    (make-attribute-map))))
    (case closing
      (#\/
       (advance)
       (make-element *root* name :attributes attrs))
      (#\>
       (let ((*root* (make-element *root* name :attributes attrs)))
         (read-children)
         *root*)))))

(defun read-tag ()
  "Attempts to read a tag and dispatches or defaults to READ-STANDARD-TAG.
Returns the completed node if one can be read."
  (if (and (char= #\< (or (consume) #\ ))
           (funcall (make-matcher :name)))   
      (let ((name (read-name)))
        (or (do-tag-dispatchers (test func (read-standard-tag name))
              (when (funcall (the function test) name)
                (return (funcall (the function func) name))))
            (progn ;; It seems we can't parse this tag for some reason,
              ;; read it as a text node instead. In order to avoid the
              ;; auto-breaking of the text node on < and a subsequently
              ;; resulting infinite-loop, don't unwind fully and instead
              ;; prepend the < manually.
              (unread-n (length name))
              (let ((text (read-text)))
                (setf (text text) (concatenate 'string "<" (text text)))
                text))))
      (progn (unread) NIL)))

(defun read-root (&optional (root (make-root)))
  "Creates a root element and reads nodes into it.
Optionally uses the specified root to append child nodes to.
Returns the root."
  (let ((*root* root))
    (loop while (peek)
          do (or (read-tag) (read-text)))
    *root*))

(defun slurp-stream (stream)
  "Quickly slurps the stream's contents into an array with fill pointer."
  (declare (stream stream))
  (with-output-to-string (string)
    (let ((buffer (make-array 4096 :element-type 'character)))
      (loop for bytes = (read-sequence buffer stream)
            do (write-sequence buffer string :start 0 :end bytes)
            while (= bytes 4096)))))

(defgeneric parse (input &key root)
  (:documentation "Parses the given input into a DOM representation.
By default, methods for STRING, PATHNAME and STREAM are defined.
If supplied, the given root is used to append children to as per READ-ROOT.
Returns the root.")
  (:method ((input string) &key root)
    (let ((input (typecase input
                   (simple-string input)
                   (string (copy-seq input)))))
      (with-lexer-environment (input)
        (if root
            (read-root root)
            (read-root)))))
  (:method ((input pathname) &key root)
    (with-open-file (stream input :direction :input)
      (parse stream :root root)))
  (:method ((input stream) &key root)
    (parse (slurp-stream input) :root root)))
