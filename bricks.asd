;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :bricks
  :serial t
  :depends-on (:iterate
                :lisputils
                :hunchentoot
                :cl-who
                :cl-ppcre
                :metabang-bind
                :veil)
  :components ((:file "package")
               (:file "core")
               (:file "forms")
               (:file "misc")))
