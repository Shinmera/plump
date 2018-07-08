#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.parser)

(declaim (inline starts-with ends-with))
(defun starts-with (find string)
  (and (<= (length find) (length string))
       (string= find string :start2 0 :end2 (length find))))

(defun ends-with (find string)
  (let ((end (length string)))
    (and (<= (length find) (length string))
         (string= find string :start2 (- end (length find)) :end2 end))))

;; We simply ignore closing tags.
;; We can do this because the matching of the proper
;; closing tag in READ-CHILDREN happens before this
;; even has a chance to dispatch. Thus only
;; inappropriate or badly ordered closing tags are
;; handled by this, which are best left ignored.
;; That way the order of the closing tags is
;; restored naturally by the reading algorithm.
(define-tag-dispatcher (invalid-closing-tag *tag-dispatchers* *xml-tags* *html-tags*) (name)
      (and (< 0 (length name)) (char= (elt name 0) #\/)))

(define-tag-parser invalid-closing-tag (name)
  (consume-until (make-matcher (is #\>)))
  (advance)
  (dolist (tag *tagstack*)
    (when (string-equal name tag :start2 1)
      (throw tag NIL)))
  NIL)

;; Comments are special nodes. We try to handle them
;; with a bit of grace, but having the inner content
;; be read in the best way possible is hard to get
;; right due to various commenting styles.
(define-tag-dispatcher (comment *tag-dispatchers* *xml-tags* *html-tags*) (name)
      (starts-with "!--" name))

(define-tag-parser comment (name)
  (make-comment
   *root*
   (decode-entities
    (if (and (ends-with "--" name)
             (char= (or (peek) #\!) #\>))
        (prog1 (subseq name 3 (max 3 (- (length name) 2)))
          (advance))
        (prog1 (concatenate
                'string (subseq name 3)
                (consume-until (make-matcher (is "-->"))))
          (advance-n 3))))))

;; Special handling for the doctype tag
(define-tag-dispatcher (doctype *tag-dispatchers* *xml-tags* *html-tags*) (name)
      (string-equal name "!DOCTYPE"))

(define-tag-parser doctype (name)
  (let ((declaration (read-tag-contents)))
    (when (char= (or (consume) #\ ) #\/)
      (advance)) ;; Consume closing
    (make-doctype *root* (string-trim " " declaration))))

;; Special handling for the XML header
(define-tag-dispatcher (xml-header *tag-dispatchers* *xml-tags*) (name)
      (string-equal name "?xml"))

(define-tag-parser xml-header (name)
  (let ((attrs (consume-until (make-matcher (and (is #\?)
                                                 (next (is #\>)))))))
    (advance-n 2)
    (make-xml-header *root* :attributes (with-lexer-environment (attrs)
                                          (read-attributes)))))

;; Special handling for CDATA sections
(define-tag-dispatcher (cdata *tag-dispatchers* *xml-tags*) (name)
      (starts-with "![CDATA[" name))

(define-tag-parser cdata (name)
  ;; KLUDGE: Since tag names can contain [ and ] we need to
  ;; take special care of cases where there is a token in the
  ;; cdata without any characters that would stop the tag
  ;; name reading.
  (let ((text (if (string= name "]]" :start1 (- (length name) 2))
                  (prog1 (subseq name 8 (- (length name) 2))
                    (advance-n 1))
                  (prog1 (concatenate 'string
                                      (subseq name 8)
                                      (consume-until (make-matcher (is "]]>"))))
                    (advance-n 3)))))
    (make-cdata *root* :text text)))

;; Shorthand macro to define self-closing elements
(defmacro define-self-closing-element (tag &rest lists)
  `(progn
     (define-tag-dispatcher (,tag ,@lists) (name)
           (string-equal name ,(string tag)))

     (define-tag-parser ,tag (name)
       (let ((attrs (read-attributes)))
         (when (char= (or (consume) #\ ) #\/)
           (advance)) ;; Consume closing
         (make-element *root* ,(string-downcase tag) :attributes attrs)))

     (define-tag-printer ,tag (node)
       (plump-dom::wrs "<" (tag-name node))
       (serialize (attributes node) *stream*)
       (if (< 0 (length (children node)))
           (progn
             (plump-dom::wrs ">")
             (loop for child across (children node)
                   do (serialize child *stream*))
             (plump-dom::wrs "</" (tag-name node) ">"))
           (plump-dom::wrs ">"))
       T)))

;; According to http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
;; area, base, br, col, embed, hr, img, input, keygen, link, menuitem, meta, param, source, track, wbr
(macrolet ((define-all (&rest tags)
             `(progn ,@(loop for tag in tags collect `(define-self-closing-element ,tag *tag-dispatchers* *html-tags*)))))
  (define-all area base br col embed hr img input keygen link menuitem meta param source track wbr))

(defmacro define-grouping-element (tag &rest lists)
  `(progn
     (define-tag-dispatcher (,tag ,@lists) (name)
       (string-equal name ,(string tag)))

     (define-tag-parser ,tag (name)
       (read-standard-tag name))
     
     (define-tag-printer ,tag (node)
       (plump-dom::wrs "<" (tag-name node))
       (serialize (attributes node) *stream*)
       (plump-dom::wrs ">")
       (loop for child across (children node)
             do (serialize child *stream*))
       (plump-dom::wrs "</" (tag-name node) ">")
       T)))

(macrolet ((define-all (&rest tags)
             `(progn ,@(loop for tag in tags collect `(define-grouping-element ,tag *tag-dispatchers* *html-tags*)))))
  (define-all blockquote dd dir div span dl dt figcaption figure hr li main ol p pre ul code pre i))

(defun read-fulltext-element-content (name)
  (with-output-to-string (out)
    (tagbody
     start (let ((next (peek)))
             (case next
               ((NIL) (go end))
               (#\< (advance) (go tag))
               (T (write-char next out) (advance) (go start))))
     tag (case (peek)
           (#\/ (advance) (go name))
           (T (write-char #\< out) (go start)))
     name (let ((tag (consume-until (make-matcher (not :name)))))
            (cond ((and (string-equal tag name) (eql #\> (peek)))
                   (advance) (go end))
                  (T
                   (write-string "</" out)
                   (write-string tag out) (go start))))
     end)))

;; Some tags accept arbitrary text and no sub-elements.
(defmacro define-fulltext-element (tag &rest lists)
  (let ((name (string-downcase tag)))
    `(progn
       (define-tag-dispatcher (,tag ,@lists) (name)
         (string-equal name ,name))

       (define-tag-parser ,tag (name)
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
              (let ((*root* (make-fulltext-element *root* ,name :attributes attrs))
                    (string (read-fulltext-element-content name)))
                (make-text-node *root* string)
                *root*)))))

       (define-tag-printer ,tag (node)
         (plump-dom::wrs "<" (tag-name node))
         (serialize (attributes node) *stream*)
         (plump-dom::wrs ">")
         (loop for child across (children node)
               do (serialize child *stream*))
         (plump-dom::wrs "</" (tag-name node) ">")
         T))))

(define-fulltext-element style *tag-dispatchers* *html-tags*)
(define-fulltext-element script *tag-dispatchers* *html-tags*)
