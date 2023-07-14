(in-package #:org.shirakumo.plump.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace* '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page)))
(defvar *root*)
(defvar *tagstack* ())

(define-matcher whitespace (find *whitespace*))
(define-matcher name (or (in #\a #\z) (in #\? #\Z) (in #\- #\:) (any #\\ #\_ #\! #\# #\[ #\])))
(define-matcher tag-end (or (and (is #\/) (next (is #\>))) (is #\>)))

(defun skip-whitespace ()
  (loop while (find (peek) *whitespace*)
        do (advance)))

(defun read-name ()
  (consume-until (make-matcher (or (not :name) :tag-end))))

(defun read-text ()
  (make-text-node
   *root*
   (decode-entities
    (consume-until (make-matcher (and (is #\<) (next :name)))))))

;; Robustify against strings inside containing >
(defun read-tag-contents ()
  (decode-entities
   (consume-until (make-matcher :tag-end))))

(defun read-children ()
  (let* ((close-tag (concatenate 'string "</" (tag-name *root*)))
         (*tagstack* (cons close-tag *tagstack*)))
    (catch close-tag
      (loop while (peek)
            do (or (read-tag) (read-text))))))

(defun read-attribute-value ()
  (decode-entities
   (let ((first (peek)))
     (case first
       (#\" (prog2 (advance)
                (consume-until (make-matcher (is #\")))
              (advance)))
       (#\' (prog2 (advance)
                (consume-until (make-matcher (is #\')))
              (advance)))
       (T (consume-until (make-matcher (or :whitespace :tag-end))))))))

(defun read-attribute-name ()
  (consume-until (make-matcher (or (is #\=) :whitespace :tag-end))))

(defun read-attribute ()
  (let ((name (read-attribute-name))
        (value ""))
    (skip-whitespace)
    (let ((next (consume)))
      (cond
        ((and next (char= next #\=))
         (skip-whitespace)
         (setf value (read-attribute-value)))
        ((not next)
         (cons name NIL))
        (T
         (unread))))
    (cons name value)))

(defun read-attributes ()
  (loop with table = (make-attribute-map)
        for char = (peek)
        do (case char
             ((#\/ #\> NIL)
              (return table))
             (#.*whitespace*
              (advance))
             (T
              (let ((entry (read-attribute)))
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun read-standard-tag (name)
  (let* ((closing (consume))
         (attrs (if (member closing *whitespace* :test #'eql)
                    (prog1 (read-attributes)
                      (setf closing (consume)))
                    (make-attribute-map))))
    (case closing
      (#\/
       (advance)
       (make-element *root* name :attributes attrs))
      (#\>
       (let ((*root* (make-element *root* name :attributes attrs)))
         (read-children)
         *root*)))))

(defun read-tag ()
  (if (and (char= #\< (or (consume) #\ ))
           (funcall (make-matcher :name)))
      (let* ((name (read-name)))
        (labels ((unread-throw (tag)
                   (unread-n (1+ (length name)))
                   (throw tag NIL))
                 (pop-implied-end-tags ()
                   "Pop up the tag stack, closing all the implicit end tags.
If there are none, return NIL."
                   ;; Better way to write it?
                   (let ((implicit nil))
                     (dolist (tag *tagstack*)
                       ;; 13.2.6.3
                       (when (member tag '("</dd" "</dt" "</li"
                                           "</optgroup" "</option"
                                           "</p" "</rb" "</rp" "</rt" "</rtc")
                                     :test #'string-equal)
                         (setf implicit tag)))
                     (when implicit
                       (unread-throw implicit)))))
          ;; TODO: 13.2.6.4.14 tr/caption/tbody
          ;; etc. wrapping. Possibly some other that slipped through.
          (or
           ;; Self-closing list elements (13.2.6.4.7).
           (and (member name '("li" "dd" "dt") :test #'string-equal)
                (pop-implied-end-tags))
           ;; Form elements are mutually exclusive (13.2.6.4.7, 13.2.6.4.17, and... common sense?)
           (let ((input-parent (or (find "</select" *tagstack* :test #'string-equal)
                                   (find "</button" *tagstack* :test #'string-equal))))
             (and input-parent
                  (member name '("input" "select" "textarea" "button" "keygen") :test #'string-equal)
                  (unread-throw input-parent)))
           ;; Implicitly closed paragraphs.
           (let ((paragraph-parent (find "</p" *tagstack* :test #'string-equal)))
             (when (member name '(;; 13.2.6.4.7
                                  "address" "article" "aside" "blockquote" "button" "center" "details"
                                  "dialog" "dir" "div" "dl" "fieldset" "figcaption" "figure" "footer"
                                  "header"  "hgroup" "listing" "main" "menu" "nav" "ol" "pre" "search"
                                  "section" "summary" "ul"
                                  ;; Same section later.
                                  "applet" "marquee" "xmp" "hr" "table" "p" "plaintext" "form" "object"
                                  "h1" "h2" "h3" "h4" "h5" "h6")
                           :test #'string-equal)
               (if paragraph-parent
                   (unread-throw paragraph-parent)
                   (pop-implied-end-tags))))
           ;; Ruby tags.
           (let ((ruby-parent (find "</ruby" *tagstack* :test #'string-equal))
                 (rtc-parent (find "</ruby" *tagstack* :test #'string-equal)))
             (cond
               ((and (or (string-equal name "rp")
                         (string-equal name "rt"))
                     rtc-parent)
                (unread-throw rtc-parent))
               ((and (member name '("rb" "rtc" "rp" "rt")
                             :test #'string-equal)
                     ruby-parent)
                (unread-throw ruby-parent))))
           ;; Wrap table cells.
           (let ((cell-parent (or (find "</td" *tagstack* :test #'string-equal)
                                  (find "</th" *tagstack* :test #'string-equal))))
             (and cell-parent
                  (member name '(;; 13.2.6.4.11
                                 "caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr")
                          :test #'string-equal)
                  (unread-throw cell-parent)))
           ;; No implicitly closed tags, proceed regularly.
           (do-tag-parsers (test func (read-standard-tag name))
             (when (funcall (the function test) name)
               (return (funcall (the function func) name))))
           (progn ;; It seems we can't parse this tag for some reason,
             ;; read it as a text node instead. In order to avoid the
             ;; auto-breaking of the text node on < and a subsequently
             ;; resulting infinite-loop, don't unwind fully and instead
             ;; prepend the < manually.
             (unread-n (length name))
             (let ((text (read-text)))
               (setf (text text) (concatenate 'string "<" (text text)))
               text)))))
      (progn (unread) NIL)))

(defun read-root (&optional (root (make-root)))
  (let ((*root* root))
    (loop while (peek)
          do (or (read-tag) (read-text)))
    *root*))

(defun slurp-stream (stream)
  (declare (stream stream))
  (with-output-to-string (string)
    (let ((buffer (make-array 4096 :element-type 'character)))
      (loop for bytes = (read-sequence buffer stream)
            do (write-sequence buffer string :start 0 :end bytes)
            while (= bytes 4096)))))

(defgeneric parse (input &key root)
  (:method ((input string) &key root)
    (let ((input (typecase input
                   (simple-string input)
                   (string (copy-seq input)))))
      (with-lexer-environment (input)
        (if root
            (read-root root)
            (read-root)))))
  (:method ((input pathname) &key root)
    (with-open-file (stream input :direction :input)
      (parse stream :root root)))
  (:method ((input stream) &key root)
    (parse (slurp-stream input) :root root)))
