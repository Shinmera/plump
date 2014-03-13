#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(defclass node ()
  ()
  (:documentation ""))

(defclass nesting-node (node)
  ((%children :initarg :children :initform (make-child-array) :accessor children))
  (:documentation ""))

(defclass child-node (node)
  ((%parent :initarg :parent :initform (error "Parent required.") :accessor parent))
  (:documentation ""))

(defclass root (nesting-node)
  ()
  (:documentation ""))

(defclass text-node (child-node)
  ((%text :initarg :text :initform "" :accessor text))
  (:documentation ""))

(defclass comment (child-node)
  ((%text :initarg :text :initform "" :accessor text))
  (:documentation ""))

(defclass element (nesting-node child-node)
  ((%tag-name :initarg :tag-name :initform (error "Tag name required.") :accessor tag-name)
   (%attributes :initarg :attributes :initform (make-attribute-map) :accessor attributes))
  (:documentation ""))

(defmethod print-object ((node element) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (write-string (tag-name node) stream))
  node)

(defclass doctype (child-node)
  ((%doctype :initarg :doctype :initform (error "Doctype declaration required.") :accessor doctype))
  (:documentation ""))

(defmethod print-object ((node doctype) stream)
  (print-unreadable-object (node stream :type T)
    (write-string (doctype node) stream))
  node)

(defun make-child-array ()
  (make-array 0 :adjustable T :fill-pointer 0))

(defun make-attribute-map ()
  (make-hash-table :test 'equalp))

(defun make-root (&optional (children (make-child-array)))
  (make-instance 'root :children children))

(defun make-element (parent tag &key (children (make-child-array)) (attributes (make-attribute-map)))
  (make-instance 'element :tag-name tag :parent parent :children children :attributes attributes))

(defun make-text-node (parent &optional (text ""))
  (make-instance 'text-node :text text :parent parent))

(defun make-comment (parent &optional (text ""))
  (make-instance 'comment :text text :parent parent))

(defun siblings (child)
  (children (parent child)))

(defun child-position (child)
  (position child (siblings child)))

(defun append-child (parent child)
  (setf (parent child) parent)
  (vector-push-extend child (children parent)))

(defun prepend-child (parent child)
  (setf (parent child) parent)
  (vector-push-extend-front child (children parent)))

(defun remove-child (child)
  (setf (parent child) NIL)
  (vector-pop-position
   (siblings child)
   (child-position child))
  child)

(defun replace-child (old-child new-child)
  (setf (parent new-child) (parent old-child)
        (elt (siblings old-child) (child-position old-child)) new-child
        (parent old-child) NIL)
  old-child)

(defun insert-before (element new-child)
  (vector-push-extend-position
   new-child
   (siblings element)
   (child-position element))
  new-child)

(defun insert-after (element new-child)
  (vector-push-extend-position
   new-child
   (siblings element)
   (1+ (child-position element)))
  new-child)

(defun clone-children (node &key deep)
  (loop with array = (make-array (length (children node)) :adjustable T :fill-pointer 0)
        for child across (children node)
        do (vector-push (if deep (clone-node child) child) array)
        finally (return array)))

(defun clone-attributes (node)
  (let ((map (make-attribute-map)))
    (loop for key being the hash-keys of (attributes node)
          for val being the hash-values of (attributes node)
          do (setf (gethash key map) val))
    map))

(defgeneric clone-node (node &key deep)
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
  (elt (children element) 0))

(defun last-child (element)
  (elt (children element) (1- (fill-pointer (children element)))))

(defun previous-sibling (child)
  (elt (siblings child)
       (1- (child-position child))))

(defun next-sibling (child)
  (elt (siblings child)
       (1+ (child-position child))))

(defun has-child-nodes (node)
  (and (slot-boundp node '%children)
       (< 0 (length (children node)))))

(defun attribute (element attribute)
  (gethash attribute (attributes element)))

(defgeneric (setf attribute) (value element attribute)
  (:method (value (element element) attribute)
    (setf (gethash attribute element) value)))

(defun get-attribute (element attribute)
  (attribute element attribute))

(defun set-attribute (element attribute value)
  (setf (attribute element attribute) value))

(defun remove-attribute (element attribute)
  (remhash attribute element))

(defun has-attribute (element attribute)
  (not (null (gethash attribute element))))

(defun get-elements-by-tag-name (node tag)
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
  (let ((finds ()))
    (labels ((scan-children (node)
               (loop for child across (children node)
                     do (when (element-p child)
                          (let ((cid (attribute child "id")))
                            (when (string-equal id cid)
                              (push child finds)))
                          (scan-children child)))))
      (scan-children node))
    finds))


(defun node-p (object)
  (typep object 'node))

(defun element-p (object)
  (typep object 'element))

(defun text-node-p (object)
  (typep object 'text-node))

(defun comment-p (object)
  (typep object 'comment))

(defun root-p (object)
  (typep object 'root))

(defvar *indent-step* 2)
(defvar *indent-level* 0)
(defun indent ()
  (make-string (* *indent-level* *indent-step*) :initial-element #\Space))

(defgeneric serialize (node stream)
  (:documentation "")
  (:method ((node text-node) stream)
    (format stream "~a" (text node)))
  (:method ((node doctype) stream)
    (format stream "<!DOCTYPE ~a>" (doctype node)))
  (:method ((node comment) stream)
    (format stream "<!--~a-->" (text node)))
  (:method ((node element) stream)
    (format stream "<~a" (tag-name node))
    (serialize (attributes node) stream)
    (if (< 0 (length (children node)))
        (progn
          (format stream ">")
          (loop for child across (children node)
                do (serialize child stream))
          (format stream "</~a>" (tag-name node)))
        (format stream "/>")))
  (:method ((table hash-table) stream)
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          do (format stream " ~a~@[=~s~]" key val)))
  (:method ((node nesting-node) stream)
    (loop for child across (children node)
          do (serialize child stream))))
