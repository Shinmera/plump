#|
  This file is a part of Colleen
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.plump.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :plump-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.plump.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object))))
  ($ template "a.funcname" (attr :href (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :plump))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :plump-parser :exclude '(:internal :method))))
      ($ "#parser-docs" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :plump-dom :exclude '(:internal :method))))
      ($ "#dom-docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :plump)))))
