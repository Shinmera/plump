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
       (when (char= (consume stream) #\/)
         (consume stream)) ;; Consume closing
       (make-element *root* ,(string-downcase tag) :attributes attrs))))

;; According to http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
;; area, base, br, col, embed, hr, img, input, keygen, link, menuitem, meta, param, source, track, wbr
(define-self-closing-element area)
(define-self-closing-element base)
(define-self-closing-element br)
(define-self-closing-element col)
(define-self-closing-element doctype)
(define-self-closing-element embed)
(define-self-closing-element hr)
(define-self-closing-element img)
(define-self-closing-element input)
(define-self-closing-element keygen)
(define-self-closing-element link)
(define-self-closing-element menuitem)
(define-self-closing-element meta)
(define-self-closing-element param)
(define-self-closing-element source)
(define-self-closing-element track)
(define-self-closing-element wbr)

