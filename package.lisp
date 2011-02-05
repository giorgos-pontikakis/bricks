(in-package :cl-user)

(defpackage :bricks
  (:use :common-lisp
        :iterate
        :lisputils
        :hunchentoot
        :cl-who
        :cl-ppcre
        :cl-fad
        :veil)
  (:export
   ;; core
   :with-html
   :defhtml
   :html
   :with-document
   :widget
   :display
   ;;forms
   :form
   :with-form
   :textbox
   :radio
   :checkbox
   :dropdown
   :label
   :submit
   ;; misc
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   :see-other))
