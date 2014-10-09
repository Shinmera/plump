#|
  This file is a part of Plump
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.plump.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.plump.asdf)

(defsystem plump
  :name "Practically Lenient and Unimpressive Markup Parser"
  :version "0.1.13"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An X/HTML parser that aims to be as lenient as possible."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package")
               (:file "array")
               (:file "indent")
               (:file "dom")
               (:file "entities")
               (:file "lexer")
               (:file "parser")
               (:file "special-tags"))
  :depends-on (:cl-ppcre))
