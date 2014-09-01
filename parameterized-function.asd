;;;; parameterized-function.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(asdf:defsystem #:parameterized-function
  :description "Compile-time parameterized functions."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:interface)
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "parameterized-function")))

