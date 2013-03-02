;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :bricks-asdf
    (:use :cl :asdf))

(in-package :bricks-asdf)

(defsystem :bricks
  :version "1.0.1"
  :serial t
  :depends-on (:alexandria :cl-who :lisputils)
  :components ((:file "package")
               (:file "core")
               (:file "widgets")
               (:file "utilities")
               (:file "forms")
               (:file "collection")
               (:file "misc")))
