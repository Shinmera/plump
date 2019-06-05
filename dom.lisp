#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.dom)

(defclass node ()
  ())

(defclass nesting-node (node)
  ((%children :initarg :children :initform (make-child-array) :accessor children :type (and (vector child-node) (not simple-array)))))
(declaim (ftype (function (nesting-node) (and (vector child-node) (not simple-array))) children))

(defclass child-node (node)
  ((%parent :initarg :parent :initform (error "Parent required.") :accessor parent :type (or null nesting-node))))
(declaim (ftype (function (child-node) (or null nesting-node)) parent))

(defclass textual-node (node)
  ((%text :initarg :text :initform "" :accessor text :type string)))

(defclass root (nesting-node)
  ())

(defclass text-node (child-node textual-node)
  ())

(defclass comment (child-node textual-node)
  ())

(defclass element (nesting-node child-node)
  ((%tag-name :initarg :tag-name :initform (error "Tag name required.") :accessor tag-name :type string)
   (%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes :type hash-table)))

(defmethod print-object ((node element) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (write-string (tag-name node) stream))
  node)

(defclass doctype (child-node)
  ((%doctype :initarg :doctype :initform (error "Doctype declaration required.") :accessor doctype :type string)))

(defmethod print-object ((node doctype) stream)
  (print-unreadable-object (node stream :type T)
    (write-string (doctype node) stream))
  node)

(defclass fulltext-element (element)
  ())

(defclass xml-header (child-node)
  ((%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes :type hash-table)))

(defmethod print-object ((node xml-header) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "version ~a" (attribute node "version")))
  node)

(defclass cdata (child-node textual-node)
  ())

(defmethod print-object ((node cdata) stream)
  (print-unreadable-object (node stream :type T)
    (if (< 20 (length (text node)))
        (format stream "~s..." (subseq (text node) 0 20))
        (format stream "~s" (text node))))
  node)

(defclass processing-instruction (child-node textual-node)
  ((%tag-name :initarg :tag-name :initform NIL :accessor tag-name :type (or null string))))

(defmethod print-object ((node processing-instruction) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~@[~a~]" (tag-name node)))
  node)

(declaim (ftype (function (&optional fixnum) (and (vector child-node) (not simple-array))) make-child-array))
(defun make-child-array (&optional (size 0))
  (make-array size :adjustable T :fill-pointer 0 :element-type 'child-node))

(declaim (ftype (function (array) (and (vector child-node) (not simple-array))) ensure-child-array))
(defun ensure-child-array (array)
  (etypecase array
    ((and vector (not simple-array))
     array)
    (vector
     (let ((proper (make-child-array (length array))))
       (loop for item across array
             do (vector-push item proper))
       proper))))

(defun make-attribute-map (&optional (size 0))
  (make-hash-table :test 'equalp :size size))

(defun ensure-attribute-map (table)
  (if (eql (hash-table-test table) 'equalp)
      table
      (let ((proper (make-attribute-map)))
        (maphash #'(lambda (k v) (setf (gethash k proper) v)) table)
        proper)))

(defun make-root (&optional (children (make-child-array)))
  (make-instance 'root :children children))

(defmacro make-appending ((class parent) &body properties)
  `(append-child
    ,parent
    (make-instance ',class :parent ,parent ,@properties)))

(defun make-element (parent tag &key (children (make-child-array)) (attributes (make-attribute-map)))
  (make-appending (element parent)
    :tag-name tag
    :children children
    :attributes attributes))

(defun make-text-node (parent &optional (text ""))
  (make-appending (text-node parent)
    :text text))

(defun make-comment (parent &optional (text ""))
  (make-appending (comment parent)
    :text text))

(defun make-doctype (parent doctype)
  (make-appending (doctype parent)
    :doctype doctype))

(defun make-fulltext-element (parent tag &key text (attributes (make-attribute-map)))
  (let ((element (make-instance 'fulltext-element :tag-name tag
                                                  :parent parent
                                                  :attributes attributes)))
    (when text
      (make-text-node element text))
    (append-child parent element)))

(defun make-xml-header (parent &key (attributes (make-attribute-map)))
  (make-appending (xml-header parent)
    :attributes attributes))

(defun make-cdata (parent &key (text ""))
  (make-appending (cdata parent)
    :text text))

(defun make-processing-instruction (parent &key name (text ""))
  (make-appending (processing-instruction parent)
    :tag-name name
    :text text))


(defmacro define-predicates (&rest classes)
  (let ((*print-case* (readtable-case *readtable*)))
    `(progn
       ,@(loop for class in classes
               for predicate = (intern (format NIL "~a-~a" (string class) (string 'p)))
               for docstring = (format NIL "Returns T if the given OBJECT is of type ~a" class)
               collect `(defun ,predicate (object)
                          ,docstring
                          (typep object ',class)) into definitions
               collect predicate into predicates
               finally (return `((declaim (inline ,@predicates))
                                 ,@definitions))))))

(define-predicates
  node
  nesting-node
  child-node
  textual-node
  root
  text-node
  comment
  element
  doctype
  fulltext-element
  xml-header
  processing-instruction
  cdata)

(defun clear (nesting-node)
  (loop for child across (children nesting-node)
        do (setf (parent child) NIL))
  (setf (fill-pointer (children nesting-node)) 0)
  nesting-node)

(defun siblings (child)
  (remove child (children (parent child))))

(defun family (child)
  (children (parent child)))

(defun child-position (child)
  (the fixnum (position child (family child))))

(defun append-child (parent child)
  (setf (parent child) parent)
  (vector-push-extend child (children parent))
  child)

(defun prepend-child (parent child)
  (setf (parent child) parent)
  (vector-push-extend-front child (children parent))
  child)

(defun remove-child (child)
  (vector-pop-position
   (family child)
   (child-position child))
  (setf (parent child) NIL)
  child)

(defun replace-child (old-child new-child)
  (setf (parent new-child) (parent old-child)
        (elt (family old-child) (child-position old-child)) new-child
        (parent old-child) NIL)
  old-child)

(defun insert-before (element new-child)
  (vector-push-extend-position
   new-child
   (family element)
   (child-position element))
  new-child)

(defun insert-after (element new-child)
  (vector-push-extend-position
   new-child
   (family element)
   (1+ (child-position element)))
  new-child)

(defun splice (element)
  (let* ((parent (parent element))
         (family (children parent))
         (count (length (children element)))
         (position (child-position element)))
    (cond ((= 0 count)
           (vector-pop-position family position)
           parent)
          (T
           (array-shift (children parent) :n (1- count) :from position)
           (loop repeat count
                 for i from position
                 for child across (children element)
                 do (setf (aref family i) child)
                    (setf (parent child) parent))))
    (setf (parent element) NIL)
    parent))

(defun clone-children (node &optional deep new-parent)
  (loop with array = (make-child-array (length (children node)))
        for child across (children node)
        do (vector-push (if deep
                            (let ((child (clone-node child T)))
                              (when new-parent (setf (parent child) new-parent))
                              child)
                            child) array)
        finally (return array)))

(defun clone-attributes (node)
  (let ((map (make-attribute-map)))
    (loop for key being the hash-keys of (attributes node)
          for val being the hash-values of (attributes node)
          do (setf (gethash key map) val))
    map))

(defgeneric clone-node (node &optional deep)
  (:method ((vector vector) &optional (deep T))
    (loop with array = (make-child-array (length vector))
          for child across vector
          do (vector-push (if deep (clone-node child T) child) array)
          finally (return array)))
  (:method ((table hash-table) &optional (deep T))
    (declare (ignore deep))
    (loop with map = (make-attribute-map)
          for key being the hash-keys of table
          for val being the hash-values of table
          do (setf (gethash key map) val)
          finally (return map)))
  (:method ((node node) &optional (deep T))
    (declare (ignore deep))
    (make-instance (class-of node)))
  (:method ((node nesting-node) &optional (deep T))
    (let ((clone (make-instance (class-of node)
                                :parent (parent node)
                                :children (make-child-array))))
      (setf (children clone) (clone-children node deep clone))
      clone))
  (:method ((node child-node) &optional (deep T))
    (declare (ignore deep))
    (make-instance (class-of node)
                   :parent (parent node)))
  (:method ((node textual-node) &optional (deep T))
    (make-instance (class-of node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node text-node) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node comment) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node element) &optional (deep T))
    (let ((clone (make-instance (class-of node)
                                :parent (parent node)
                                :tag-name (tag-name node)
                                :children (make-child-array)
                                :attributes (clone-attributes node))))
      (setf (children clone) (clone-children node deep clone))
      clone))
  (:method ((node doctype) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :doctype (if deep (copy-seq (doctype node)) (doctype node))))
  (:method ((node xml-header) &optional (deep T))
    (declare (ignore deep))
    (make-instance (class-of node)
                   :parent (parent node)
                   :attributes (clone-attributes node)))
  (:method ((node cdata) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node processing-instruction) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :tag-name (tag-name node)
                   :text (if deep (copy-seq (text node)) (text node)))))

(defun first-child (element)
  (when (< 0 (fill-pointer (children element)))
    (elt (children element) 0)))

(defun last-child (element)
  (when (< 0 (fill-pointer (children element)))
    (elt (children element) (1- (fill-pointer (children element))))))

(defun previous-sibling (child)
  (let ((pos (child-position child)))
    (when (< 0 pos)
      (elt (family child) (1- pos)))))

(defun next-sibling (child)
  (let ((pos (1+ (child-position child)))
        (family (family child)))
    (when (< pos (fill-pointer family))
      (elt family pos))))

(defun vec-remove-if (predicate sequence)
  (loop with vector = (make-child-array)
        for child across sequence
        unless (funcall predicate child)
        do (vector-push-extend child vector)
        finally (return vector)))

(defun child-elements (nesting-node)
  (vec-remove-if #'(lambda (c) (not (element-p c))) (children nesting-node)))

(defun element-position (child)
  (loop with position = 0
        for sibling across (family child)
        until (eq sibling child)
        when (element-p sibling)
          do (incf position)
        finally (return position)))

(defun sibling-elements (child)
  (vec-remove-if #'(lambda (sibling)
                     (or (eq sibling child)
                         (not (element-p sibling))))
                 (family child)))

(defun family-elements (child)
  (child-elements (parent child)))

(defun first-element (element)
  (when (< 0 (fill-pointer (children element)))
    (loop for child across (children element)
          when (element-p child) do (return child))))

(defun last-element (element)
  (when (< 0 (fill-pointer (children element)))
    (let ((last (elt (children element) (1- (fill-pointer (children element))))))
      (if (element-p last)
          last
          (previous-element last)))))

(defun previous-element (child)
  (let ((pos (1- (child-position child)))
        (family (family child)))
    (loop while (< pos (fill-pointer family))
          for current = (elt family pos)
          do (decf pos)
          when (element-p current)
            do (return-from previous-element current))))

(defun next-element (child)
  (let ((pos (1+ (child-position child)))
        (family (family child)))
    (loop while (< pos (fill-pointer family))
          for current = (elt family pos)
          do (incf pos)
          when (element-p current)
            do (return-from next-element current))))

(defun has-child-nodes (node)
  (and (nesting-node-p node)
       (< 0 (length (children node)))))

(defun attribute (element attribute)
  (gethash attribute (attributes element)))

(defun (setf attribute) (value element attribute)
  (setf (gethash attribute (attributes element)) value))

(defun get-attribute (element attribute)
  (attribute element attribute))

(defun set-attribute (element attribute value)
  (setf (attribute element attribute) value))

(defun remove-attribute (element attribute)
  (remhash attribute (attributes element))
  NIL)

(defun has-attribute (element attribute)
  (nth-value 1 (gethash attribute (attributes element))))

(defmethod text ((node nesting-node))
  "Compiles all text nodes within the nesting-node into one string."
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (children node)
                     do (typecase child
                          (textual-node (write-string (text child) stream))
                          (nesting-node (r child))))))
      (r node))))

(defun render-text (node)
  (with-output-to-string (stream)
    (let ((have-space NIL)
          (char-func #'identity))
      (labels ((beginning (char)
                 (case char
                   ((#\Space #\Tab #\Return #\Linefeed))
                   (T (setf char-func #'body)
                    (write-char char stream))))
               (body (char)
                 (case char
                   ((#\Space #\Tab #\Return #\Linefeed)
                    (setf have-space T))
                   (T
                    (when have-space
                      (write-char #\Space stream)
                      (setf have-space NIL))
                    (write-char char stream))))
               (r (node)
                 (typecase node
                   (textual-node (loop for c across (text node) do (funcall char-func c)))
                   (nesting-node (loop for c across (children node) do (r c))))))
        (setf char-func #'beginning)
        (r node)))))

(defun get-elements-by-tag-name (node tag)
  (let ((finds ()))
    (labels ((scanren (node)
               (loop for child across (children node)
                     do (when (element-p child)
                          (when (string-equal tag (tag-name child))
                            (push child finds))
                          (scanren child)))))
      (scanren node))
    finds))

(defun get-element-by-id (node id)
  (labels ((scanren (node)
             (loop for child across (children node)
                   do (when (element-p child)
                        (let ((cid (attribute child "id")))
                          (when (string-equal id cid)
                            (return-from get-element-by-id child)))
                        (scanren child)))))
    (scanren node))
  NIL)

(defvar *stream*)

(defun serialize (node &optional (stream T))
  (cond ((eql stream T)
         (let ((*stream* *standard-output*))
           (serialize-object node)))
        ((eql stream NIL)
         (with-output-to-string (*stream*)
           (serialize-object node)))
        (T
         (let ((*stream* stream))
           (serialize-object node)))))

(defmacro wrs (&rest strings)
  `(progn
     ,@(loop for string in strings
             collect `(write-string ,string *stream*))))

(defgeneric serialize-object (node)
  (:method ((node text-node))
    (encode-entities (text node) *stream*))
  (:method ((node doctype))
    (wrs "<!DOCTYPE " (doctype node) ">"))
  (:method ((node comment))
    (wrs "<!--" (text node) "-->"))
  (:method ((node element))
    (or (plump-parser:do-tag-printers (test printer)
          (when (funcall test (tag-name node))
            (return (funcall printer node))))
        (progn
          (wrs "<" (tag-name node))
          (serialize (attributes node) *stream*)
          (if (< 0 (length (children node)))
              (progn
                (wrs ">")
                (loop for child across (children node)
                      do (serialize-object child))
                (wrs "</" (tag-name node) ">"))
              (wrs "/>")))))
  (:method ((node fulltext-element))
    (wrs "<" (tag-name node))
    (serialize-object (attributes node))
    (wrs ">")
    (loop for child across (children node)
          when (text-node-p child)
          do (wrs (text child)))
    (wrs "</" (tag-name node) ">"))
  (:method ((node xml-header))
    (wrs "<?xml")
    (serialize-object (attributes node))
    (wrs "?>"))
  (:method ((node cdata))
    (wrs "<![CDATA[" (text node) "]]>"))
  (:method ((node processing-instruction))
    (wrs "<?")
    (when (tag-name node)
      (wrs (tag-name node)))
    (wrs " " (text node) "?>"))
  (:method ((table hash-table))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          do (wrs " " key)
             (when val
               (wrs "=\"")
               (encode-entities val *stream*)
               (wrs "\""))))
  (:method ((node nesting-node))
    (loop for child across (children node)
          do (serialize-object child)))
  (:method ((nodes vector))
    (loop for child across nodes
          do (serialize-object child))))

(defgeneric traverse (node function &key test)
  (:method ((node node) function &key (test (constantly T)))
    (when (funcall test node)
      (funcall function node))
    node)
  (:method ((node nesting-node) function &key (test (constantly T)))
    (call-next-method)
    (loop for child across (copy-seq (children node))
          do (traverse child function :test test))
    node))

(defun trim (node)
  (traverse
   node
   #'(lambda (node)
       (setf (text node) (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) (text node))))
   :test #'text-node-p))

(defun strip (node)
  (traverse
   node
   #'(lambda (node)
       (setf (text node) (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) (text node)))
       (when (string= (text node) "")
         (remove-child node)))
   :test #'text-node-p))
