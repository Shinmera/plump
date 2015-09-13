#|
  This file is a part of Plump
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plump
  :name "Practically Lenient and Unimpressive Markup Parser"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An XML / XHTML / HTML parser that aims to be as lenient as possible."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package"))
  :depends-on (:plump-dom
               :plump-lexer
               :plump-parser))
