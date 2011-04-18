;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :bricks
  :serial t
  :depends-on (:iterate
                :lisputils
                :alexandria
                :hunchentoot
                :cl-who
                :cl-ppcre
                :cl-fad
                :veil)
  :components ((:file "package")
               (:file "core")
               (:file "paths")
               (:file "utilities")
               (:file "forms")
               (:file "collection")
               (:file "misc")
               (:file "variations")))
