#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl)

(defpackage #:plump-lexer
  (:nicknames #:org.shirakumo.plump.lexer)
  (:use #:cl)
  (:export
   #:*string*
   #:*length*
   #:*index*
   #:with-lexer-environment
   #:consume
   #:advance
   #:unread
   #:peek
   #:advance-n
   #:unread-n
   #:consume-until
   #:matcher-character
   #:matcher-string
   #:matcher-range
   #:matcher-find
   #:matcher-or
   #:matcher-and
   #:matcher-not
   #:matcher-next
   #:matcher-prev
   #:matcher-any
   #:make-matcher
   #:define-matcher))

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
   #:render-text
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
   #:strip)
  ;; entities.lisp
  (:export
   #:*entity-map*
   #:translate-entity
   #:decode-entities
   #:allowed-char-p
   #:discouraged-char-p
   #:invalid-xml-character
   #:discouraged-xml-character
   #:faulty-char
   #:write-encode-char
   #:use-new-character
   #:encode-entities))

(defpackage #:plump-parser
  (:nicknames #:org.shirakumo.plump.parser)
  (:use #:cl #:plump-lexer #:plump-dom)
  ;; parser.lisp
  (:export
   #:*root*
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
   #:parse
   #:slurp-stream)
  ;; processing.lisp
  (:export
   #:processing-parser
   #:remove-processing-parser
   #:define-processing-parser)
  ;; special-tags.lisp
  (:export
   #:define-self-closing-element
   #:define-fulltext-element)
  ;; tag-dispatcher.lisp
  (:export
   #:*all-tag-dispatchers*
   #:*tag-dispatchers*
   #:*xml-tags*
   #:*html-tags*
   #:tag-dispatcher
   #:remove-tag-dispatcher
   #:define-tag-dispatcher
   #:define-tag-parser
   #:define-tag-printer
   #:do-tag-parsers
   #:do-tag-printers))

(defpackage #:plump
  (:nicknames #:org.shirakumo.plump)
  (:use #:cl #:plump-dom #:plump-lexer #:plump-parser))

(in-package #:plump)
(let ((plump (find-package "PLUMP")))
  (do-external-symbols (symb (find-package "PLUMP-LEXER"))
    (export symb plump))
  (do-external-symbols (symb (find-package "PLUMP-PARSER"))
    (export symb plump))
  (do-external-symbols (symb (find-package "PLUMP-DOM"))
    (export symb plump)))
