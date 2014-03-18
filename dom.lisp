#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defclass node ()
  ()
  (:documentation "Base DOM node class."))

(defclass nesting-node (node)
  ((%children :initarg :children :initform (make-child-array) :accessor children))
  (:documentation "Node class that can contain child nodes."))

(defclass child-node (node)
  ((%parent :initarg :parent :initform (error "Parent required.") :accessor parent))
  (:documentation "Node class that is a child and thus has a parent."))

(defclass root (nesting-node)
  ()
  (:documentation "Root DOM node, practically equivalent to a \"document\"."))

(defclass text-node (child-node)
  ((%text :initarg :text :initform "" :accessor text))
  (:documentation "Text node that can only contain a single text string."))

(defclass comment (child-node)
  ((%text :initarg :text :initform "" :accessor text))
  (:documentation "Comment node that can only contain a single comment string."))

(defclass element (nesting-node child-node)
  ((%tag-name :initarg :tag-name :initform (error "Tag name required.") :accessor tag-name)
   (%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes))
  (:documentation "Standard DOM element/block including attributes, tag-name, parent and children."))

(defmethod print-object ((node element) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (write-string (tag-name node) stream))
  node)

(defclass doctype (child-node)
  ((%doctype :initarg :doctype :initform (error "Doctype declaration required.") :accessor doctype))
  (:documentation "Special DOM node for the doctype declaration."))

(defmethod print-object ((node doctype) stream)
  (print-unreadable-object (node stream :type T)
    (write-string (doctype node) stream))
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

(defun clone-children (node &key deep)
  "Clone the array of children.
If DEEP is non-NIL, each child is cloned as per
 (CLONE-NODE :DEEP T) "
  (loop with array = (make-array (length (children node)) :adjustable T :fill-pointer 0)
        for child across (children node)
        do (vector-push (if deep (clone-node child :deep T) child) array)
        finally (return array)))

(defun clone-attributes (node)
  "Clone the attribute map.
Note that keys and values are NOT copied/cloned."
  (let ((map (make-attribute-map)))
    (loop for key being the hash-keys of (attributes node)
          for val being the hash-values of (attributes node)
          do (setf (gethash key map) val))
    map))

(defgeneric clone-node (node &key deep)
  (:documentation "Clone the given node, creating a new instance with the same contents.
If DEEP is non-NIL, the following applies:
The text of COMMENT and TEXT-NODEs is copied as per COPY-SEQ.
The children of NESTING-NODEs are copied as per (CLONE-CHILDREN :DEEP T)")
  (:method ((node comment) &key (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node text-node) &key (deep T))
    (make-instance (class-of node)
                   :parent (parent node)
                   :text (if deep (copy-seq (text node)) (text node))))
  (:method ((node nesting-node) &key deep)
    (make-instance (class-of node)
                   :children (clone-children node :deep deep)))
  (:method ((node element) &key deep)
    (make-instance (class-of node)
                   :tag-name (tag-name node)
                   :children (clone-children node :deep deep)
                   :attributes (clone-attributes node))))

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
  (let ((pos (1+ (child-position child))))
    (when (< pos (fill-pointer (family child)))
      (elt (family child) pos))))

(defun has-child-nodes (node)
  "Returns T if the node can contain children and
the child array is not empty."
  (and (slot-boundp node '%children)
       (< 0 (length (children node)))))

(defun attribute (element attribute)
  "Returns the asked attribute from the element
or NIL. If the attribute could not be found, the
second return value is set to NIL."
  (gethash attribute (attributes element)))

(defgeneric (setf attribute) (value element attribute)
  (:documentation "Set an attribute on an element to the given value.")
  (:method (value (element element) attribute)
    (setf (gethash attribute element) value)))

(defun get-attribute (element attribute)
  "Synonymous to ATTRIBUTE."
  (attribute element attribute))

(defun set-attribute (element attribute value)
  "Synonymous to (SETF (ATTIBUTE ..) ..)"
  (setf (attribute element attribute) value))

(defun remove-attribute (element attribute)
  "Remove the specified attribute if it exists.
Returns NIL."
  (remhash attribute element)
  NIL)

(defun has-attribute (element attribute)
  "Returns T if the provided attribute exists."
  (nth-value 1 (gethash attribute element)))

(defun get-elements-by-tag-name (node tag)
  "Searches the given node and returns an unordered
list of child nodes at arbitrary depth that match
the given tag."
  (let ((finds ()))
    (labels ((scan-children (node)
               (loop for child across (children node)
                     do (when (element-p child)
                          (when (string-equal tag (tag-name child))
                            (push child finds))
                          (scan-children child)))))
      (scan-children node))
    finds))

(defun get-element-by-id (node id)
  "Searches the given node and returns the first
node at arbitrary depth that matches the given ID
attribute."
  (labels ((scan-children (node)
             (loop for child across (children node)
                   do (when (element-p child)
                        (let ((cid (attribute child "id")))
                          (when (string-equal id cid)
                            (return-from get-element-by-id child)))
                        (scan-children child)))))
    (scan-children node))
  NIL)


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

(defvar *indent-step* 2)
(defvar *indent-level* 0)
(defun indent ()
  (make-string (* *indent-level* *indent-step*) :initial-element #\Space))

(defgeneric serialize (node &optional stream)
  (:documentation "Serialize the given node and print it to the stream.")
  (:method ((node text-node) &optional (stream *standard-output*))
    (format stream "~a" (encode-entities (text node))))
  (:method ((node doctype) &optional (stream *standard-output*))
    (format stream "<!DOCTYPE ~a>" (doctype node)))
  (:method ((node comment) &optional (stream *standard-output*))
    (format stream "<!--~a-->" (text node)))
  (:method ((node element) &optional (stream *standard-output*))
    (format stream "<~a" (tag-name node))
    (serialize (attributes node) stream)
    (if (< 0 (length (children node)))
        (progn
          (format stream ">")
          (loop for child across (children node)
                do (serialize child stream))
          (format stream "</~a>" (tag-name node)))
        (format stream "/>")))
  (:method ((table hash-table) &optional (stream *standard-output*))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          do (format stream " ~a~@[=~s~]" key (when val (encode-entities val)))))
  (:method ((node nesting-node) &optional (stream *standard-output*))
    (loop for child across (children node)
          do (serialize child stream))))
