(in-package :bricks)

(defun see-other (url)
  (redirect url :code +http-see-other+))

(defun css (file)
  (with-html
    (:link :href file
           :rel "stylesheet"
           :type "text/css")))

(defun js (file)
  (with-html
    (:script :type "text/javascript"
             :src file)))

(defun img (file)
  (with-html
    (:img :src (url 'img file))))
