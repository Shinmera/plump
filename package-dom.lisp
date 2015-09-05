#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:plump-dom
  (:use #:cl #:array-utils)
  (:nicknames #:org.shirakumo.plump.dom)
  ;; dom.lisp
  (:export
   #:node
   #:nesting-node
   #:children
   #:child-node
   #:parent
   #:textual-node
   #:text
   #:root
   #:text-node
   #:comment
   #:element
   #:tag-name
   #:attributes
   #:doctype
   #:doctype
   #:fulltext-element
   #:xml-header
   #:processing-instruction
   #:cdata
   #:make-child-array
   #:ensure-child-array
   #:make-attribute-map
   #:ensure-attribute-map
   #:make-root
   #:make-element
   #:make-text-node
   #:make-comment
   #:make-doctype
   #:make-fulltext-element
   #:make-xml-header
   #:make-cdata
   #:make-processing-instruction
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
   #:splice
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
   #:get-elements-by-tag-name
   #:get-element-by-id
   #:node-p
   #:nesting-node-p
   #:child-node-p
   #:textual-node-p
   #:root-p
   #:text-node-p
   #:comment-p
   #:element-p
   #:doctype-p
   #:fulltext-element-p
   #:xml-header-p
   #:processing-instruction-p
   #:cdata-p
   #:*stream*
   #:serialize
   #:serialize-object
   #:traverse
   #:trim
   #:strip
   #:xml-character-p)
  ;; entities.lisp
  (:export
   #:*entity-map*
   #:translate-entity
   #:decode-entities
   #:encode-entities))
