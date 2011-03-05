(in-package :bricks)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form ()
  ((submit-page :accessor submit-page :initarg :submit-page)
   (action      :accessor action      :initarg :action)
   (hidden      :accessor hidden      :initarg :hidden)
   (body        :accessor body        :initarg :body))
  (:default-initargs :hidden nil))

(defmethod display ((form form) &key)
  (let ((page (find-page (submit-page form) (package-webapp))))
    (with-html
      (:form :method (request-type page)
             :action (action form)
             (iter (for key in (hidden form) by #'cddr)
                   (for val in (rest (hidden form)) by #'cddr)
                   (when val
                     (htm
                      (:input :type "hidden"
                              :id (string-downcase key)
                              :style "display: none;"
                              :name (string-downcase key)
                              :value (lisp->html val)))))
             (display (body form))))))

(defmacro with-form (url &body body)
  (let* ((pos (position-if #'keywordp url))
         (hidden (if pos (subseq url pos) nil)))
    `(display (make-instance 'form
                             :submit-page ',(first url)
                             :action ,(subseq url 0 pos)
                             :hidden (list ,@hidden)
                             :body (html ()
                                     ,@body)))))


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
    (:ul :id id
         :class style
         (iter (for (label value) in label-value-alist)
               (htm (:li (:input :type "radio"
                                 :name (string-downcase name)
                                 :value (lisp->html value)
                                 :checked (equal value checked)
                                 :readonly readonlyp
                                 :disabled disabledp)
                         (display label)))))))

(defun checkbox (name label &key value id style readonlyp disabledp checked)
  (with-html
    (:input :type "checkbox"
            :id id
            :class style
            :name (string-downcase name)
            :value (lisp->html value)
            :checked (equal value checked)
            :readonly readonlyp
            :disabled disabledp
            (display label))))

(defun dropdown (name label-value-alist &key id style readonlyp disabledp selected)
  (with-html
    (:select :id id
             :class style
             :name (string-downcase name)
             :disabled disabledp
             (iter (for (label value) in label-value-alist)
                   (htm (:option :value (lisp->html value)
                                 :selected (equal value selected)
                                 :readonly readonlyp
                                 (display label)))))))

(defun label (name text &key style)
  (with-html
    (:label :class style
            :for (string-downcase name)
            (display text))))

(defun submit (label &key name value style disabledp)
  (with-html
    (:button :class style
             :type "submit"
             :name (if name (string-downcase name) nil)
             :value (if value (lisp->html value) nil)
             :disabled disabledp
             (display label))))
