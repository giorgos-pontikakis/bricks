(in-package :bricks)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defun form (submit-page hidden body)
  (let ((page (find-page submit-page (package-webapp))))
    (with-html
      (:form :method (request-type page)
             :action (concatenate 'string (web-root (webapp page)) (base-url page))
             (iter (for key in hidden by #'cddr)
                   (for val in (rest hidden) by #'cddr)
                   (htm
                     (:input :type "hidden"
                             :id (string-downcase key)
                             :style "display: none;"
                             :name (string-downcase key)
                             :value (lisp->html val))))
             (render body)))))

(defmacro with-form (url &body body)
  (let ((page-name (first url))
        (hidden (rest url)))
    `(form ',page-name (list ,@hidden)
           ,@body)))


(defun textbox (name &key id style readonlyp disabledp passwordp value)
  (with-html
    (:input :id id
            :class style
            :type (if passwordp "password" "text")
            :name (string-downcase name)
            :value (lisp->html (or value :null))
            :readonly readonlyp
            :disabled disabledp)))

(defun radio (name label-value-alist &key id style readonlyp disabledp checked)
  (with-html
    (:ul :id (or id (string-downcase name))
         :class style
         (iter (for (label value) in label-value-alist)
               (htm (:li (:input :type "radio"
                                 :name (string-downcase name)
                                 :value (lisp->html value)
                                 :checked (equal value checked)
                                 :readonly readonlyp
                                 :disabled disabledp)
                         (render label)))))))

(defun dropdown (name label-value-alist &key style readonlyp disabledp selected)
  (with-html
    (:select :id (string-downcase name)
             :class style
             :name (string-downcase name)
             :disabled disabledp
             (iter (for (label value) in label-value-alist)
                   (htm (:option :value (lisp->html value)
                                 :selected (equal value selected)
                                 :readonly readonlyp
                                 (render label)))))))

(defun label (name text &key style)
  (with-html
    (:label :class style
            :for (string-downcase name)
            (render text))))

(defun submit (label &key name value style disabledp)
  (with-html
    (:button :class style
             :type "submit"
             :name (if name (string-downcase name) nil)
             :value (if value (lisp->html value) nil)
             :disabled disabledp
             (render label))))
