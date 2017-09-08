#|
  This file is a part of Plump
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem plump
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An XML / XHTML / HTML parser that aims to be as lenient as possible."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package")
               (:file "entities")
               (:file "lexer")
               (:file "tag-dispatcher")
               (:file "dom")
               (:file "parser")
               (:file "processing")
               (:file "special-tags")
               (:file "documentation"))
  :depends-on (:array-utils
               :documentation-utils))
