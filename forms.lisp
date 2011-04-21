(in-package :bricks)



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form ()
  ((submit-page :reader submit-page :initarg :submit-page)
   (action      :reader action      :initarg :action)
   (hidden      :reader hidden      :initarg :hidden)
   (body        :reader body        :initarg :body))
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



;;; ------------------------------------------------------------
;;; Form elements
;;; ------------------------------------------------------------

(defclass form-element (widget)
  ((disabled :reader disabled :initarg :disabled)))



;;; ------------------------------------------------------------
;;; text input boxes
;;; ------------------------------------------------------------

(defclass input-text (form-element)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (readonly :reader readonly :initarg :readonly)
   (password :reader password :initarg :password)))

(defmethod display ((input-text input-text) &key id style value readonly disabled password)
  (with-html
    (:input :id (or id (id input-text) (string-downcase (name input-text)))
            :class (or style (style input-text))
            :type (if (or password (password input-text)) "password" "text")
            :name (string-downcase (name input-text))
            :value (lisp->html (or value (value input-text) :null))
            :readonly (or readonly (readonly input-text))
            :disabled (or disabled (disabled input-text)))))

(defun input-text (name &key id style readonly disabled password value)
  (display (make-instance 'input-text
                          :id id
                          :style style
                          :disabled disabled
                          :name name
                          :value value
                          :readonly readonly
                          :password password)))



;;; ------------------------------------------------------------
;;; checkbox/radio input boxes
;;; ------------------------------------------------------------

(defclass input-checkbox/radio (form-element)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (content  :reader content  :initarg :content)
   (checked  :reader checked  :initarg :checked)
   (readonly :reader readonly :initarg :readonly)))

(defclass input-radio (input-checkbox/radio)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox (input-checkbox/radio)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((checkable input-checkbox/radio) &key)
  (with-html
    (:input :id (id checkable)
            :class (style checkable)
            :type (kind checkable)
            :name (string-downcase (name checkable))
            :value (value checkable)
            :readonly (readonly checkable)
            :disabled (disabled checkable)
            :checked (checked checkable)
            (str (content checkable)))))

(defun input-radio (name value content &key id style checked readonly disabled)
  (display (make-instance 'radio
                          :id id
                          :style style
                          :disabled disabled
                          :name name
                          :value value
                          :content content
                          :checked checked
                          :readonly readonly)))

(defun input-checkbox (name value content &key id style checked readonly disabled)
  (display (make-instance 'checkbox
                          :id id
                          :style style
                          :disabled disabled
                          :name name
                          :value value
                          :content content
                          :checked checked
                          :readonly readonly)))



;;; ------------------------------------------------------------
;;; radio/checkbox input sets
;;; ------------------------------------------------------------

(defclass input-checkbox/radio-set (form-element)
  ((kind              :reader kind              :initarg :kind)
   (name              :reader name              :initarg :name)
   (label-value-alist :reader label-value-alist :initarg :label-value-alist)
   (checked           :reader checked           :initarg :checked)
   (readonly          :reader readonly          :initarg :readonly)))

(defclass input-radio-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((input-set input-checkbox/radio-set) &key)
  (with-html
    (:ul :id (id input-set)
         :class (style input-set)
         (iter (for (label value) in (label-value-alist input-set))
               (htm (:li (:input :type (string-downcase (kind input-set))
                                 :name (string-downcase (name input-set))
                                 :value (lisp->html (value input-set))
                                 :checked (equal value (checked input-set))
                                 :readonly (readonly input-set)
                                 :disabled (disabled input-set)
                                 (display label))))))))

(defun input-checkbox-set (name label-value-alist &key id style checked readonly disabled)
  (display (make-instance 'input-checkbox-set
                          :name name
                          :label-value-alist label-value-alist
                          :id id
                          :style style
                          :checked checked
                          :readonly readonly
                          :disabled disabled)))

(defun input-radio-set (name label-value-alist &key id style checked readonly disabled)
  (display (make-instance 'input-radio-set
                          :name name
                          :label-value-alist label-value-alist
                          :id id
                          :style style
                          :checked checked
                          :readonly readonly
                          :disabled disabled)))



;;; ------------------------------------------------------------
;;; dropdown menus
;;; ------------------------------------------------------------

(defclass dropdown (form-element)
  ((name              :reader name              :initarg :name)
   (label-value-alist :reader label-value-alist :initarg :label-value-alist)
   (selected          :reader selected          :initarg :selected)
   (readonly          :reader readonly          :initarg :readonly)
   (disabled          :reader disabled          :initarg :disabled)))

(defmethod display ((dropdown dropdown) &key)
  (with-html
    (:select :id (id dropdown)
             :class (style dropdown)
             :name (string-downcase (name dropdown))
             :disabled (disabled dropdown)
             (iter (for (label value) in (label-value-alist dropdown))
                   (htm (:option :value (lisp->html value)
                                 :selected (equal value (selected dropdown))
                                 :readonly (readonly dropdown)
                                 (display label)))))))

(defun dropdown (name label-value-alist &key id style readonly disabled selected)
  (make-instance 'dropdown
                 :id id
                 :style style
                 :name name
                 :label-value-alist label-value-alist
                 :readonly readonly
                 :disabled disabled
                 :selected selected))



;;; ------------------------------------------------------------
;;; buttons
;;; ------------------------------------------------------------

(defclass button (form-element)
  ((kind      :initform "button")
   (content   :reader content   :initarg :content)
   (name      :reader name      :initarg :name)
   (value     :reader value     :initarg :value)
   (style     :reader style     :initarg :style)
   (disabled  :reader disabled :initarg :disabled)))

(defclass submit (button)
  ((kind :reader kind :initform "submit")))

(defmethod display ((button button) &key)
  (with-html
    (:button :id (id button)
             :class (style button)
             :type (kind button)
             :name (if-let (name (name button))
                     (string-downcase name)
                     nil)
             :value (if-let (value (value button))
                      (lisp->html value)
                      nil)
             :disabled (disabled button)
             (display (content button)))))

(defun button (content &key id style name value disabled)
  (display (make-instance 'button
                          :content content
                          :id id
                          :style style
                          :name name
                          :value value
                          :disabled disabled)))

(defun submit (content &key id style name value disabled)
  (display (make-instance 'submit
                          :content content
                          :id id
                          :style style
                          :name name
                          :value value
                          :disabled disabled)))



;;; ------------------------------------------------------------
;;; label
;;; ------------------------------------------------------------

(defun label (name text &key style)
  (with-html
    (:label :class style
            :for (string-downcase name)
            (display text))))