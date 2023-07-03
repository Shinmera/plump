(asdf:defsystem plump
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
