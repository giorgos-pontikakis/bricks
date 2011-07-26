(in-package :bricks)

(defun css (href)
  (with-html
    (:link :href href
           :rel "stylesheet"
           :type "text/css")))

(defun js (href)
  (with-html
    (:script :type "text/javascript"
             :src href)))
