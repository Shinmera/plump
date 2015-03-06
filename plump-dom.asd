#|
 This file is a part of Plump
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plump-dom
  :name "Plump-DOM"
  :version "1.2.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A DOM for use with the Plump parser."
  :homepage "https://github.com/Shinmera/plump"
  :serial T
  :components ((:file "package-dom")
               (:file "entities")
               (:file "dom"))
  :depends-on (:array-utils))
