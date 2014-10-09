#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.plump)

;; We simply ignore closing tags.
;; We can do this because the matching of the proper
;; closing tag in READ-CHILDREN happens before this
;; even has a chance to dispatch. Thus only
;; inappropriate or badly ordered closing tags are
;; handled by this, which are best left ignored.
;; That way the order of the closing tags is
;; restored naturally by the reading algorithm.
(define-tag-dispatcher invalid-closing-tag (name)
      (and (< 0 (length name)) (char= (elt name 0) #\/))
  (consume-until (make-matcher (is #\>)))
  (advance)
  NIL)

;; Comments are special nodes. We try to handle them
;; with a bit of grace, but having the inner content
;; be read in the best way possible is hard to get
;; right due to various commenting styles.
(define-tag-dispatcher comment (name)
      (and (<= 3 (length name))
           (string= name "!--" :end1 3))
  (prog1 (make-comment
          *root*
          (decode-entities
           (concatenate
            'string (subseq name 3)
            (consume-until (make-matcher (is "-->"))))))
    (advance-n 3)))

;; Special handling for the doctype tag
(define-tag-dispatcher doctype (name)
      (string-equal name "!DOCTYPE")
  (let ((declaration (read-tag-contents)))
    (when (char= (consume) #\/)
      (advance)) ;; Consume closing
    (make-doctype *root* (string-trim " " declaration))))

;; Special handling for the XML header
(define-tag-dispatcher xml-header (name)
      (string-equal name "?xml")
  (let ((attrs (consume-until (make-matcher (and (is #\?)
                                                 (next (is #\>)))))))
    (advance-n 2)
    (make-xml-header *root* :attributes (with-lexer-environment (attrs)
                                          (read-attributes)))))

;; Shorthand macro to define self-closing elements
(defmacro define-self-closing-element (tag)
  `(define-tag-dispatcher ,tag (name)
         (string-equal name ,(string tag))
     (let ((attrs (read-attributes)))
       (when (char= (consume) #\/)
         (advance)) ;; Consume closing
       (make-element *root* ,(string-downcase tag) :attributes attrs))))

;; According to http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
;; area, base, br, col, embed, hr, img, input, keygen, link, menuitem, meta, param, source, track, wbr
(define-self-closing-element area)
(define-self-closing-element base)
(define-self-closing-element br)
(define-self-closing-element col)
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

;; Some tags accept arbitrary text and no sub-elements.
(defmacro define-fulltext-element (tag)
  (let ((name (string-downcase tag)))
    `(define-tag-dispatcher ,tag (name) (string-equal name ,name)
       (let* ((closing (consume))
              (attrs (if (char= closing #\Space)
                         (prog1 (read-attributes)
                           (setf closing (consume)))
                         (make-attribute-map))))
         (case closing
           (#\/
            (advance)
            (make-element *root* ,name :attributes attrs))
           (#\>
            (let ((*root* (make-fulltext-element *root* ,name :attributes attrs)))
              (make-text-node *root* (consume-until (make-matcher (is ,(format NIL "</~a>" name)))))
              (advance-n ,(length (format NIL "</~a>" name)))
              *root*)))))))

(define-fulltext-element style)
(define-fulltext-element script)
