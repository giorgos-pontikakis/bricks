;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :bricks-asdf
  (:use :cl :asdf))

(in-package :bricks-asdf)

(defsystem :bricks
  :version "2.0.0"
  :serial t
  ;;
  :depends-on (:alexandria
               :cl-who
               (:version :lisputils "1.1.0"))
  ;;
  :components ((:file "package")
               (:file "core")
               (:file "widgets")
               (:file "forms")
               (:file "records")
               (:file "collections")
               (:file "crud")
               (:file "misc")))
