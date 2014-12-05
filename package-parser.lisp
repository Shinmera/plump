#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl)
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
   #:*tag-dispatchers*
   #:*xml-tags*
   #:*html-tags*
   #:tag-dispatcher
   #:set-tag-dispatcher
   #:define-tag-dispatcher
   #:remove-tag-dispatcher
   #:do-tag-dispatchers))
