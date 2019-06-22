#|
  This file is a part of Plump
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem plump
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An XML / XHTML / HTML parser that aims to be as lenient as possible."
  :homepage "https://Shinmera.github.io/plump/"
  :bug-tracker "https://github.com/Shinmera/plump/issues"
  :source-control (:git "https://github.com/Shinmera/plump.git")
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
