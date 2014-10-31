#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defclass node ()
  ()
  (:documentation "Base DOM node class."))

(defclass nesting-node (node)
  ((%children :initarg :children :initform (make-child-array) :accessor children :type vector))
  (:documentation "Node class that can contain child nodes."))

(defclass child-node (node)
  ((%parent :initarg :parent :initform (error "Parent required.") :accessor parent :type (or null node)))
  (:documentation "Node class that is a child and thus has a parent."))

(defclass root (nesting-node)
  ()
  (:documentation "Root DOM node, practically equivalent to a \"document\"."))

(defclass text-node (child-node)
  ((%text :initarg :text :initform "" :accessor text :type string))
  (:documentation "Text node that can only contain a single text string."))

(defclass comment (child-node)
  ((%text :initarg :text :initform "" :accessor text :type string))
  (:documentation "Comment node that can only contain a single comment string."))

(defclass element (nesting-node child-node)
  ((%tag-name :initarg :tag-name :initform (error "Tag name required.") :accessor tag-name :type string)
   (%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes :type hash-table))
  (:documentation "Standard DOM element/block including attributes, tag-name, parent and children."))

(defmethod print-object ((node element) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (write-string (tag-name node) stream))
  node)

(defclass doctype (child-node)
  ((%doctype :initarg :doctype :initform (error "Doctype declaration required.") :accessor doctype :type string))
  (:documentation "Special DOM node for the doctype declaration."))

(defmethod print-object ((node doctype) stream)
  (print-unreadable-object (node stream :type T)
    (write-string (doctype node) stream))
  node)

(defclass fulltext-element (element) ()
  (:documentation "Special DOM element that contains full, un-entitied text like SCRIPT and STYLE."))

(defclass xml-header (child-node)
  ((%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes :type hash-table))
  (:documentation "XML header element"))

(defmethod print-object ((node xml-header) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "version ~a" (attribute node "version")))
  node)

(defclass cdata (child-node)
  ((%text :initarg :text :initform "" :accessor text :type string))
  (:documentation "XML CDATA section node."))

(defmethod print-object ((node cdata) stream)
  (print-unreadable-object (node stream :type T))
  node)

(defclass processing-instruction (child-node)
  ((%tag-name :initarg :tag-name :initform NIL :accessor tag-name :type (or null string))
   (%text :initarg :text :initform "" :accessor text :type string))
  (:documentation "XML processing instruction node."))

(defmethod print-object ((node processing-instruction) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~@[~a~]" (tag-name node)))
  node)

(defun make-child-array ()
  "Creates an array to contain child elements"
  (make-array 0 :adjustable T :fill-pointer 0))

(defun make-attribute-map ()
  "Creates a map to contain attributes."
  (make-hash-table :test 'equalp))

(defun make-root (&optional (children (make-child-array)))
  "Creates a root node with the given children.
Children should be a vector with fill-pointer."
  (make-instance 'root :children children))

(defun make-element (parent tag &key (children (make-child-array)) (attributes (make-attribute-map)))
  "Creates a standard DOM element with the given tag name and parent.
Optionally a vector (with fill-pointer) containing children and an
attribute-map (a hash-table with equalp test) can be supplied.

Note that the element is automatically appended to the parent's child list."
  (append-child parent (make-instance 'element :tag-name tag :parent parent :children children :attributes attributes)))

(defun make-text-node (parent &optional (text ""))
  "Creates a new text node under the parent.

Note that the node is automatically appended to the parent's child list."
  (append-child parent (make-instance 'text-node :text text :parent parent)))

(defun make-comment (parent &optional (text ""))
  "Creates a new comment node under the parent.

Note that the node is automatically appended to the parent's child list."
  (append-child parent (make-instance 'comment :text text :parent parent)))

(defun make-doctype (parent doctype)
  "Creates a new doctype node under the parent.

Note that the node is automatically appended to the parent's child list."
  (append-child parent (make-instance 'doctype :doctype doctype :parent parent)))

(defun make-fulltext-element (parent tag &key text (attributes (make-attribute-map)))
  "Creates a fulltext element under the parent.
Optionally a text and an attribute-map (a hash-table with equalp test) 
can be supplied.

Note that the element is automatically appended to the parent's child list."
  (let ((element (make-instance 'fulltext-element :tag-name tag :parent parent :attributes attributes)))
    (when text
      (make-text-node element text))
    (append-child parent element)))

(defun make-xml-header (parent &key (attributes (make-attribute-map)))
  "Creates an XML header object under the parent.

Note that the element is automatically appended to the parent's child list."
  (append-child parent (make-instance 'xml-header :attributes attributes :parent parent)))

(defun make-cdata (parent &key (text ""))
  "Creates an XML CDATA section under the parent.

Note that the element is automatically appended to the parent's child list."
  (append-child parent (make-instance 'cdata :text text :parent parent)))

(defun make-processing-instruction (parent &key name (text ""))
  "Creates an XML processing instruction under the parent.

Note that the element is automatically appended to the parent's child list."
  (append-child parent (make-instance 'processing-instruction :tag-name name :text text :parent parent)))

(defun clear (nesting-node)
  "Clears all children from the node.

Noe that the PARENT of all child elements is set to NIL."
  (loop for child across (children nesting-node)
        do (setf (parent child) NIL))
  (setf (fill-pointer (children nesting-node)) 0)
  nesting-node)

(defun siblings (child)
  "Returns the array of siblings of the given child.
Note that this is a copy of the array, modifying it is safe."
  (remove child (children (parent child))))

(defun family (child)
  "Returns the direct array of children of the parent of the given child.
Note that modifying this array directly modifies that of the parent."
  (children (parent child)))

(defun child-position (child)
  "Returns the index of the child within its parent."
  (position child (family child)))

(defun append-child (parent child)
  "Appends the given child onto the parent's child list.
Returns the child."
  (setf (parent child) parent)
  (vector-push-extend child (children parent))
  child)

(defun prepend-child (parent child)
  "Prepends the given child onto the parent's child list.
Returns the child.

Note that this operation is costly, see VECTOR-PUSH-EXTEND-FRONT"
  (setf (parent child) parent)
  (vector-push-extend-front child (children parent))
  child)

(defun remove-child (child)
  "Removes the child from its parent.
Returns the child.

Note that this operation is potentially very costly.
See VECTOR-POP-POSITION"
  (vector-pop-position
   (family child)
   (child-position child))
  (setf (parent child) NIL)
  child)

(defun replace-child (old-child new-child)
  "Replace the old child with a new one.
Returns the old child."
  (setf (parent new-child) (parent old-child)
        (elt (family old-child) (child-position old-child)) new-child
        (parent old-child) NIL)
  old-child)

(defun insert-before (element new-child)
  "Inserts the new-child before the given element in the parent's list.
Returns the new child.

Note that this operation is potentially very costly.
See VECTOR-PUSH-EXTEND-POSITION"
  (vector-push-extend-position
   new-child
   (family element)
   (child-position element))
  new-child)

(defun insert-after (element new-child)
  "Inserts the new-child after the given element in the parent's list.
Returns the new child.

Note that this operation is potentially very costly.
See VECTOR-PUSH-EXTEND-POSITION"
  (vector-push-extend-position
   new-child
   (family element)
   (1+ (child-position element)))
  new-child)

(defun clone-children (node &optional deep new-parent)
  "Clone the array of children.
If DEEP is non-NIL, each child is cloned as per (CLONE-NODE NODE T).
When copying deeply, you can also pass a NEW-PARENT to set on each child."
  (loop with array = (make-array (length (children node)) :adjustable T :fill-pointer 0)
        for child across (children node)
        do (vector-push (if deep
                            (let ((child (clone-node child T)))
                              (when new-parent (setf (parent child) new-parent))
                              child)
                            child) array)
        finally (return array)))

(defun clone-attributes (node)
  "Clone the attribute map.
Note that keys and values are NOT copied/cloned."
  (let ((map (make-attribute-map)))
    (loop for key being the hash-keys of (attributes node)
          for val being the hash-values of (attributes node)
          do (setf (gethash key map) val))
    map))

(defgeneric clone-node (node &optional deep)
  (:documentation "Clone the given node, creating a new instance with the same contents.
If DEEP is non-NIL, the following applies:
The text of COMMENT and TEXT-NODEs is copied as per COPY-SEQ.
The children of NESTING-NODEs are copied as per (CLONE-CHILDREN CHILD T)")
  (:method ((node comment) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node text-node) &optional (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node nesting-node) &optional (deep T))
    (let ((clone (make-instance (class-of node)
                                :parent (parent node)
                                :children (make-array 0))))
      (setf (children clone) (clone-children node deep clone))
      clone))
  (:method ((node element) &optional (deep T))
    (let ((clone (make-instance (class-of node)
                                :parent (parent node)
                                :tag-name (tag-name node)
                                :children (make-array 0)
                                :attributes (clone-attributes node))))
      (setf (children clone) (clone-children node deep clone))
      clone)))

(defun first-child (element)
  "Returns the first child within the parent or NIL
if the parent is empty."
  (when (< 0 (fill-pointer (children element)))
    (elt (children element) 0)))

(defun last-child (element)
  "Returns the last child within the parent or NIL
if the parent is empty."
  (when (< 0 (fill-pointer (children element)))
    (elt (children element) (1- (fill-pointer (children element))))))

(defun previous-sibling (child)
  "Returns the sibling before this one or NIL if 
it is already the first."
  (let ((pos (child-position child)))
    (when (< 0 pos)
      (elt (family child) (1- pos)))))

(defun next-sibling (child)
  "Returns the sibling next to this one or NIL if
it is already the last."
  (let ((pos (1+ (child-position child)))
        (family (family child)))
    (when (< pos (fill-pointer family))
      (elt family pos))))

(defun vec-remove-if (predicate sequence)
  (loop with vector = (make-array 0 :adjustable T :fill-pointer 0)
        for child across sequence
        unless (funcall predicate child)
          do (vector-push-extend child vector)
        finally (return vector)))

(defun child-elements (nesting-node)
  "Returns a new vector of children of the given node, filtered to elements."
  (vec-remove-if #'(lambda (c) (not (element-p c))) (children nesting-node)))

(defun element-position (child)
  "Returns the index of the child within its parent, counting only elements.
This excludes comments, text-nodes and the like."
  (loop with position = 0
        for sibling across (family child)
        until (eq sibling child)
        when (element-p sibling)
          do (incf position)
        finally (return position)))

(defun sibling-elements (child)
  "Returns the array of sibling elements of the given child.
Note that this is a copy of the array, modifying it is safe.
This excludes comments, text-nodes and the like."
  (vec-remove-if #'(lambda (sibling)
                     (or (eq sibling child)
                         (not (element-p sibling))))
                 (family child)))

(defun family-elements (child)
  "Returns the direct array of children elements of the parent of the given child.
Note that this is a copy of the array, modifying it is safe.
This excludes comments, text-nodes and the like."
  (child-elements (parent child)))

(defun first-element (element)
  "Returns the first child element within the parent or NIL
if the parent is empty. This excludes comments, text-nodes and the like."
  (when (< 0 (fill-pointer (children element)))
    (loop for child across (children element)
          when (element-p child) do (return child))))

(defun last-element (element)
  "Returns the last child element within the parent or NIL
if the parent is empty. This excludes comments, text-nodes and the like."
  (when (< 0 (fill-pointer (children element)))
    (let ((last (elt (children element) (1- (fill-pointer (children element))))))
      (if (element-p last)
          last
          (previous-element last)))))

(defun previous-element (child)
  "Returns the sibling element next to this one or NIL if
it is already last. This excludes comments, text-nodes and the like."
  (let ((pos (1- (child-position child)))
        (family (family child)))
    (loop while (< pos (fill-pointer family))
          for current = (elt family pos)
          do (decf pos)
          when (element-p current)
            do (return-from previous-element current))))

(defun next-element (child)
  "Returns the sibling element next to this one or NIL if
it is already last. This excludes comments, text-nodes and the like."
  (let ((pos (1+ (child-position child)))
        (family (family child)))
    (loop while (< pos (fill-pointer family))
          for current = (elt family pos)
          do (incf pos)
          when (element-p current)
            do (return-from next-element current))))

(defun has-child-nodes (node)
  "Returns T if the node can contain children and
the child array is not empty."
  (and (nesting-node-p node)
       (< 0 (length (children node)))))

(defun attribute (element attribute)
  "Returns the asked attribute from the element
or NIL. If the attribute could not be found, the
second return value is set to NIL."
  (gethash attribute (attributes element)))

(defgeneric (setf attribute) (value element attribute)
  (:documentation "Set an attribute on an element to the given value.")
  (:method (value (element element) attribute)
    (setf (gethash attribute (attributes element)) value)))

(defun get-attribute (element attribute)
  "Synonymous to ATTRIBUTE."
  (attribute element attribute))

(defun set-attribute (element attribute value)
  "Synonymous to (SETF (ATTIBUTE ..) ..)"
  (setf (attribute element attribute) value))

(defun remove-attribute (element attribute)
  "Remove the specified attribute if it exists.
Returns NIL."
  (remhash attribute (attributes element))
  NIL)

(defun has-attribute (element attribute)
  "Returns T if the provided attribute exists."
  (nth-value 1 (gethash attribute (attributes element))))

(defmethod text ((node nesting-node))
  "Compiles all text nodes within the nesting-node into one string."
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (children node)
                     do (typecase child
                          (text-node (write-string (text child) stream))
                          (nesting-node (r child))))))
      (r node))))

(defun get-elements-by-tag-name (node tag)
  "Searches the given node and returns an unordered
list of child nodes at arbitrary depth that match
the given tag."
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
  "Searches the given node and returns the first
node at arbitrary depth that matches the given ID
attribute."
  (labels ((scanren (node)
             (loop for child across (children node)
                   do (when (element-p child)
                        (let ((cid (attribute child "id")))
                          (when (string-equal id cid)
                            (return-from get-element-by-id child)))
                        (scanren child)))))
    (scanren node))
  NIL)

(declaim (inline node-p element-p text-node-p comment-p root-p nesting-node-p))
(defun node-p (object)
  "Returns T if the given object is a NODE."
  (typep object 'node))

(defun element-p (object)
  "Returns T if the given object is an ELEMENT."
  (typep object 'element))

(defun text-node-p (object)
  "Returns T if the given object is a TEXT-NODE."
  (typep object 'text-node))

(defun comment-p (object)
  "Returns T if the given object is a COMMENT."
  (typep object 'comment))

(defun root-p (object)
  "Returns T if the given object is a ROOT."
  (typep object 'root))

(defun nesting-node-p (object)
  "Returns T if the given object is a NESTING-NODE."
  (typep object 'nesting-node))

(defun fulltext-element-p (object)
  "Returns T If the given object is a FULLTEXT-ELEMENT."
  (typep object 'fulltext-element))

(defvar *stream* *standard-output*
  "The stream to serialize to during SERIALIZE-OBJECT.")

(defun serialize (node &optional (stream T))
  "Serializes NODE to STREAM.
STREAM can be a stream, T for *standard-output* or NIL to serialize to string."
  (cond ((eql stream T)
         (let ((*stream* *standard-output*))
           (serialize-object node)))
        ((eql stream NIL)
         (with-output-to-string (*stream*)
           (serialize-object node)))
        (T
         (let ((*stream* stream))
           (serialize-object node)))))

(defgeneric serialize-object (node)
  (:documentation "Serialize the given node and print it to *stream*.")
  (:method ((node text-node))
    (format *stream* "~a" (encode-entities (text node))))
  (:method ((node doctype))
    (format *stream* "<!DOCTYPE ~a>" (doctype node)))
  (:method ((node comment))
    (format *stream* "<!--~a-->" (text node)))
  (:method ((node element))
    (format *stream* "<~a" (tag-name node))
    (serialize (attributes node) *stream*)
    (if (< 0 (length (children node)))
        (progn
          (format *stream* ">")
          (loop for child across (children node)
                do (serialize child *stream*))
          (format *stream* "</~a>" (tag-name node)))
        (format *stream* "/>")))
  (:method ((node fulltext-element))
    (format *stream* "<~a" (tag-name node))
    (serialize (attributes node) *stream*)
    (if (< 0 (length (children node)))
        (progn
          (format *stream* ">")
          (loop for child across (children node)
                when (text-node-p child)
                  do (format *stream* "~a" (text child)))
          (format *stream* "</~a>" (tag-name node)))
        (format *stream* "/>")))
  (:method ((node xml-header))
    (format *stream* "<?xml")
    (serialize (attributes node) *stream*)
    (format *stream* "?>"))
  (:method ((node cdata))
    (format *stream* "<![CDATA[~a]]>" (text node)))
  (:method ((node processing-instruction))
    (format *stream* "<?~@[~a~]~a ?>" (tag-name node) (text node)))
  (:method ((table hash-table))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          do (format *stream* " ~a~@[=~s~]" key (when val (encode-entities val)))))
  (:method ((node nesting-node))
    (loop for child across (children node)
          do (serialize child *stream*)))
  (:method ((nodes vector))
    (loop for child across nodes
          do (serialize child *stream*))))
