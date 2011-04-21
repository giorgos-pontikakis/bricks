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
  ((disabled :reader disabled :initarg :disabled))
  (:default-initargs :disabled nil))



;;; ------------------------------------------------------------
;;; text input boxes
;;; ------------------------------------------------------------

(defclass input-text (form-element)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (readonly :reader readonly :initarg :readonly)
   (password :reader password :initarg :password))
  (:default-initargs :value nil :readonly nil :password nil))

(defmethod display ((input-text input-text) &key
                    id style name value
                    (readonly nil readonly-s) (disabled nil disabled-s) (password nil password-s))
  (let ((password-p (if password-s password (password input-text))))
    (with-html
      (:input :id (or id (id input-text))
              :class (or style (style input-text))
              :type (if password-p "password" "text")
              :name (string-downcase (or name (name input-text)))
              :value (lisp->html (or value (value input-text) :null))
              :readonly (if readonly-s readonly (readonly input-text))
              :disabled (if disabled-s disabled (disabled input-text))))))

(defun input-text (name &rest instance-initargs)
  (display (apply #'make-instance 'input-text
                  :name name
                  instance-initargs)))



;;; ------------------------------------------------------------
;;; checkbox/radio input boxes
;;; ------------------------------------------------------------

(defclass input-checkbox/radio (form-element)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (content  :reader content  :initarg :content)
   (checked  :reader checked  :initarg :checked)
   (readonly :reader readonly :initarg :readonly))
  (:default-initargs :checked nil :readonly nil))

(defclass input-radio (input-checkbox/radio)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox (input-checkbox/radio)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((checkable input-checkbox/radio) &key
                    id style name value content
                    (checked nil checked-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:input :id (or id (id checkable))
            :class (or style (style checkable))
            :type (kind checkable)
            :name (string-downcase (or name (name checkable)))
            :value (or value (value checkable))
            :readonly (if readonly-s readonly (readonly checkable))
            :disabled (if disabled-s disabled (disabled checkable))
            :checked (if checked-s checked (checked checkable))
            (str (or content (content checkable))))))

(defun input-radio (name value content &rest instance-initargs)
  (display (apply #'make-instance 'radio
                  :name name
                  :value value
                  :content content
                  instance-initargs)))

(defun input-checkbox (name value content &rest instance-initargs)
  (display (apply #'make-instance 'input-checkbox
                  :name name
                  :value value
                  :content content
                  instance-initargs)))



;;; ------------------------------------------------------------
;;; radio/checkbox input sets
;;; ------------------------------------------------------------

(defclass input-checkbox/radio-set (form-element)
  ((kind              :reader kind              :initarg :kind)
   (name              :reader name              :initarg :name)
   (label-value-alist :reader label-value-alist :initarg :label-value-alist)
   (checked           :reader checked           :initarg :checked)
   (readonly          :reader readonly          :initarg :readonly))
  (:default-initargs :checked nil :readonly nil :disabled nil))

(defclass input-radio-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((input-set input-checkbox/radio-set) &key
                    id style name label-value-alist
                    (checked nil checked-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:ul :id (or id (id input-set))
         :class (or style (style input-set))
         (iter (for (label value) in (or label-value-alist (label-value-alist input-set)))
               (htm (:li (:input :type (string-downcase (kind input-set))
                                 :name (string-downcase (or name (name input-set)))
                                 :value (lisp->html value)
                                 :checked (if checked-s checked (equal value (checked input-set)))
                                 :readonly (if readonly-s readonly (readonly input-set))
                                 :disabled (if disabled-s disabled (disabled input-set))
                                 (display label))))))))

(defun input-checkbox-set (name label-value-alist &rest instance-initargs)
  (display (apply #'make-instance 'input-checkbox-set
                  :name name
                  :label-value-alist label-value-alist
                  instance-initargs)))

(defun input-radio-set (name label-value-alist &rest instance-initargs)
  (display (apply #'make-instance 'input-radio-set
                  :name name
                  :label-value-alist label-value-alist
                  instance-initargs)))



;;; ------------------------------------------------------------
;;; dropdown menus
;;; ------------------------------------------------------------

(defclass dropdown (form-element)
  ((name              :reader name              :initarg :name)
   (label-value-alist :reader label-value-alist :initarg :label-value-alist)
   (selected          :reader selected          :initarg :selected)
   (readonly          :reader readonly          :initarg :readonly)
   (disabled          :reader disabled          :initarg :disabled))
  (:default-initargs :selected nil :readonly nil :disabled nil))

(defmethod display ((dropdown dropdown) &key
                    id style name label-value-alist
                    (selected nil selected-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:select :id (or id (id dropdown))
             :class (or style (style dropdown))
             :name (string-downcase (or name (name dropdown)))
             :disabled (if disabled-s disabled (disabled dropdown))
             (iter (for (label value) in (or label-value-alist (label-value-alist dropdown)))
                   (htm (:option :value (lisp->html value)
                                 :selected (equal value
                                                  (if selected-s selected (selected dropdown)))
                                 :readonly (if readonly-s readonly (readonly dropdown))
                                 (display label)))))))

(defun dropdown (name label-value-alist &rest instance-initargs)
  (display (apply #'make-instance 'dropdown
                  :name name
                  :label-value-alist label-value-alist
                  instance-initargs)))



;;; ------------------------------------------------------------
;;; buttons
;;; ------------------------------------------------------------

(defclass button (form-element)
  ((kind      :reader kind     :initform "button")
   (content   :reader content  :initarg :content)
   (name      :reader name     :initarg :name)
   (value     :reader value    :initarg :value)
   (disabled  :reader disabled :initarg :disabled))
  (:default-initargs :name nil :value nil :disabled nil))

(defclass submit (button)
  ((kind :reader kind :initform "submit")))

(defmethod display ((button button) &key
                    id style content name value
                    (disabled nil disabled-s))
  (with-html
    (:button :id (or id (id button))
             :class (or style (style button))
             :type (kind button)
             :name (if-let (n (or name (name button)))
                     (string-downcase n)
                     nil)
             :value (if-let (v (or value (value button)))
                      (lisp->html v)
                      nil)
             :disabled (if disabled-s disabled (disabled button))
             (display (or content (content button))))))

(defun button (content &rest instance-initargs)
  (display (apply #'make-instance 'button :content content instance-initargs)))

(defun submit (content &rest instance-initargs)
  (display (apply #'make-instance 'submit :content content instance-initargs)))



;;; ------------------------------------------------------------
;;; label
;;; ------------------------------------------------------------

(defun label (name content &key id style)
  (with-html
    (:label :id id
            :class style
            :for (string-downcase name)
            (display content))))