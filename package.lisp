#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:plump-dom
  (:nicknames #:org.tymoonnext.plump.dom)
  ;; dom.lisp
  (:export
   #:node
   #:nesting-node
   #:children
   #:child-node
   #:parent
   #:root
   #:text-node
   #:text
   #:comment
   #:text
   #:element
   #:tag-name
   #:attributes
   #:doctype
   #:doctype
   #:fulltext-element
   #:make-child-array
   #:make-attribute-map
   #:make-root
   #:make-element
   #:make-text-node
   #:make-comment
   #:make-doctype
   #:make-fulltext-element
   #:clear
   #:siblings
   #:family
   #:child-position
   #:append-child
   #:prepend-child
   #:remove-child
   #:replace-child
   #:insert-before
   #:insert-after
   #:clone-children
   #:clone-attributes
   #:clone-node
   #:first-child
   #:last-child
   #:previous-sibling
   #:next-sibling
   #:element-position
   #:child-elements
   #:sibling-elements
   #:family-elements
   #:first-element
   #:last-element
   #:previous-element
   #:next-element
   #:has-child-nodes
   #:attribute
   #:get-attribute
   #:set-attribute
   #:remove-attribute
   #:has-attribute
   #:ge-elements-by-tag-name
   #:get-element-by-id
   #:node-p
   #:element-p
   #:text-node-p
   #:comment-p
   #:root-p
   #:nesting-node-p
   #:fulltext-element-p
   #:serialize))

(defpackage #:plump-parser
  (:nicknames #:org.tymoonnext.plump.parser)
  ;; entities.lisp
  (:export
   #:*entity-map*
   #:translate-entity
   #:decode-entities)
  ;; lexer.lisp
  (:export
   #:with-lexer-environment
   #:consume
   #:advance
   #:unread
   #:peek
   #:advance-n
   #:unread-n
   #:consume-until
   #:matcher-string
   #:matcher-or
   #:matcher-and
   #:matcher-not
   #:make-matcher
   #:define-matcher)
  ;; parser.lisp
  (:export
   #:*root*
   #:*tag-dispatchers*
   #:define-tag-dispatcher
   #:read-name
   #:read-text
   #:read-tag-contents
   #:read-children
   #:read-attribute-value
   #:read-attribute-name
   #:read-attribute
   #:read-attributes
   #:read-standard-tag
   #:read-tag
   #:read-root
   #:parse)
  ;; special-tags.lisp
  (:export
   #:define-self-closing-element
   #:define-fulltext-element))

(defpackage #:plump
  (:nicknames #:org.tymoonnext.plump)
  (:use #:cl #:plump-dom #:plump-parser))

(in-package #:plump)
(let ((plump (find-package "PLUMP")))
  (do-external-symbols (symb (find-package "PLUMP-PARSER"))
    (export symb plump))
  (do-external-symbols (symb (find-package "PLUMP-DOM"))
    (export symb plump)))
