;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :bricks
  :serial t
  :depends-on (:alexandria :cl-who :lisputils)
  :components ((:file "package")
               (:file "core")
               (:file "widgets")
               (:file "utilities")
               (:file "forms")
               (:file "collection")
               (:file "misc")
               (:file "variations")))
