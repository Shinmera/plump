#|
  This file is a part of Plump
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.plump.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.plump.asdf)

(defsystem plump
  :name "Practically Lenient and Unimpressive Markup Parser"
  :version "0.9.6"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An X/HTML parser that aims to be as lenient as possible."
  :serial T
  :components ((:file "package")
               (:file "array")
               (:file "dom")
               (:file "lexer"))
  :depends-on ())
