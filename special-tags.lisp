#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

(define-tag-dispatcher comment (name stream)
                       (and (<= 3 (length name))
                            (string= name "!--" :end1 3))
  (read-comment stream))

(defmacro define-self-closing-element (tag)
  `(define-tag-dispatcher ,tag (name stream)
                          (string-equal name ,(string tag))
     (let ((attrs (read-attributes stream)))
       (consume stream)
       (make-element *root* ,(string tag) :attributes attrs))))

(define-self-closing-element doctype)
(define-self-closing-element meta)
(define-self-closing-element link)
