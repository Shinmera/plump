#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem plump-parser
  :name "Practically Lenient and Unimpressive Markup Parser"
  :version "0.9.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Plump's core parser component."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package-parser")
               (:file "tag-dispatcher")
               (:file "parser")
               (:file "processing")
               (:file "special-tags"))
  :depends-on (:trivial-indent
               :plump-dom
               :plump-lexer))
