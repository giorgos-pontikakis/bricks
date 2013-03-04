;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :bricks-asdf
    (:use :cl :asdf))

(in-package :bricks-asdf)

(defsystem :bricks
  :version "1.0.3"
  :serial t
  ;;
  :depends-on (:alexandria
               :cl-who
               (:version :lisputils "1.0.0"))
  ;;
  :components ((:file "package")
               (:file "core")
               (:file "widgets")
               (:file "utilities")
               (:file "forms")
               (:file "collection")
               (:file "misc")))
