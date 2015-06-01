#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plump-lexer
  :name "Plump-Lexer"
  :version "1.0.2"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A very simple toolkit to help with lexing used mainly in Plump."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package-lexer")
               (:file "lexer"))
  :depends-on ())
