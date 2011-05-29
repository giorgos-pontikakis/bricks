(in-package :bricks)



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form ()
  ((action  :reader action  :initarg :action)
   (reqtype :reader reqtype :initarg :reqtype)
   (hidden  :reader hidden  :initarg :hidden)
   (body    :reader body    :initarg :body))
  (:default-initargs :hidden nil :reqtype "GET"))

(defmethod display ((form form) &key action reqtype hidden body)
  (with-html
    (:form :action (or action (action form))
           :method (or reqtype (reqtype form))
           (plist-mapc (lambda (key val)
                         (when val
                           (htm
                            (:input :type "hidden"
                                    :name (string-downcase key)
                                    :value (lisp->html val)))))
                       (or hidden (hidden form)))
           (display (or body (body form))))))

(defun form (action body &rest instance-initargs &key reqtype hidden)
  (declare (ignore reqtype hidden))
  (display (apply #'make-instance 'form
                  :action action
                  :body body
                  instance-initargs)))

(defmacro with-form (url &body body)
  (let* ((pos (position-if #'keywordp url))
         (hidden (if pos (subseq url pos) nil)))
    `(display (make-instance 'form
                             :action ,(subseq url 0 pos)
                             :reqtype (request-type (find-page ',(first url) (package-webapp)))
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
                    id css-class name value
                    (readonly nil readonly-s) (disabled nil disabled-s) (password nil password-s))
  (let ((password-p (if password-s password (password input-text))))
    (with-html
      (:input :id (or id (id input-text))
              :class (or css-class (css-class input-text))
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
   (body  :reader body  :initarg :body)
   (checked  :reader checked  :initarg :checked)
   (readonly :reader readonly :initarg :readonly))
  (:default-initargs :checked nil :readonly nil))

(defclass input-radio (input-checkbox/radio)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox (input-checkbox/radio)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((checkable input-checkbox/radio) &key
                    id css-class name value body
                    (checked nil checked-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:input :id (or id (id checkable))
            :class (or css-class (css-class checkable))
            :type (kind checkable)
            :name (string-downcase (or name (name checkable)))
            :value (lisp->html (or value (value checkable)))
            :readonly (if readonly-s readonly (readonly checkable))
            :disabled (if disabled-s disabled (disabled checkable))
            :checked (if checked-s checked (checked checkable))
            (str (or body (body checkable))))))

(defun input-radio (name value body &rest instance-initargs)
  (display (apply #'make-instance 'input-radio
                  :name name
                  :value value
                  :body body
                  instance-initargs)))

(defun input-checkbox (name value body &rest instance-initargs)
  (display (apply #'make-instance 'input-checkbox
                  :name name
                  :value value
                  :body body
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
                    id css-class name label-value-alist
                    (checked nil checked-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:ul :id (or id (id input-set))
         :class (or css-class (css-class input-set))
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
                    id css-class name label-value-alist
                    (selected nil selected-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:select :id (or id (id dropdown))
             :class (or css-class (css-class dropdown))
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
  ((kind     :reader kind     :initform "button")
   (body     :reader body     :initarg  :body)
   (name     :reader name     :initarg  :name)
   (value    :reader value    :initarg  :value)
   (disabled :reader disabled :initarg  :disabled))
  (:default-initargs :name nil :value nil :disabled nil))

(defclass submit (button)
  ((kind :reader kind :initform "submit")))

(defmethod display ((button button) &key
                    id css-class body name value
                    (disabled nil disabled-s))
  (with-html
    (:button :id (or id (id button))
             :class (or css-class (css-class button))
             :type (kind button)
             :name (if-let (n (or name (name button)))
                     (string-downcase n)
                     nil)
             :value (if-let (v (or value (value button)))
                      (lisp->html v)
                      nil)
             :disabled (if disabled-s disabled (disabled button))
             (display (or body (body button))))))

(defun button (body &rest instance-initargs)
  (display (apply #'make-instance 'button :body body instance-initargs)))

(defun submit (body &rest instance-initargs)
  (display (apply #'make-instance 'submit :body body instance-initargs)))



;;; ------------------------------------------------------------
;;; label
;;; ------------------------------------------------------------

(defun label (name body &key id css-class)
  (with-html
    (:label :id id
            :class css-class
            :for (string-downcase name)
            (display body))))