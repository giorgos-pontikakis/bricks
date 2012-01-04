(in-package :bricks)



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form (widget)
  ((action  :reader action  :initarg :action)
   (reqtype :reader reqtype :initarg :reqtype)
   (hidden  :reader hidden  :initarg :hidden)
   (body    :reader body    :initarg :body))
  (:default-initargs :hidden nil :reqtype "GET"))

(defmethod display ((form form) &key id css-class action reqtype hidden body)
  (with-html
    (:form :action (or action (action form))
           :method (or reqtype (reqtype form))
           :id (or id (id form))
           :css-class (or css-class (css-class form))
           (plist-mapc (lambda (key val)
                         (when val
                           (htm
                            (:input :type "hidden"
                                    :name (string-downcase key)
                                    :value (lisp->html val)))))
                       (or hidden (hidden form)))
           (display (or body (body form))))))

(defun form (action body &key id css-class reqtype hidden)
  (display (make-instance 'form
                          :id id
                          :css-class css-class
                          :reqtype reqtype
                          :action action
                          :body body
                          :hidden hidden)))



;;; ------------------------------------------------------------
;;; Form elements
;;; ------------------------------------------------------------

(defclass form-element (widget)
  ((disabled :reader disabled :initarg :disabled))
  (:default-initargs :disabled nil))

(defclass input (form-element)
  ((readonly :accessor readonly :initarg :readonly))
  (:default-initargs :readonly nil))

;;; ------------------------------------------------------------
;;; text input boxes
;;; ------------------------------------------------------------

(defclass input-text (input)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (password :reader password :initarg :password))
  (:default-initargs :name (error 'slot-uninitialized :class 'input-text :slot 'name)
                     :value nil
                     :password nil))

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

(defun input-text (name &rest initargs &key id css-class disabled readonly value password)
  (declare (ignore id css-class disabled readonly value password))
  (display (apply #'make-instance 'input-text :name name initargs)))



;;; ------------------------------------------------------------
;;; checkbox/radio input boxes
;;; ------------------------------------------------------------

(defclass input-checkbox/radio (input)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (body     :reader body     :initarg :body)
   (checked  :reader checked  :initarg :checked))
  (:default-initargs :name (error 'slot-uninitialized :class 'input-checkbox/radio :slot 'name)
                     :value (error 'slot-uninitialized :class 'input-checkbox/radio :slot 'value)
                     :body (error 'slot-uninitialized :class 'input-checkbox/radio :slot 'body)
                     :checked nil))

(defclass input-radio (input-checkbox/radio)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox (input-checkbox/radio)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((checkable input-checkbox/radio) &key
                    id css-class name value body
                    (disabled nil disabled-s) (readonly nil readonly-s) (checked nil checked-s))
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

(defun input-radio (name value body &rest initargs &key id css-class disabled readonly checked)
  (declare (ignore id css-class disabled readonly checked))
  (display (apply #'make-instance 'input-radio :name name
                                               :value value
                                               :body body
                                               initargs)))

(defun input-checkbox (name value body &rest initargs &key id css-class disabled readonly checked)
  (declare (ignore id css-class disabled readonly checked))
  (display (apply #'make-instance 'input-checkbox :name name
                                                  :value value
                                                  :body body
                                                  initargs)))



;;; ------------------------------------------------------------
;;; radio/checkbox input sets
;;; ------------------------------------------------------------

(defclass input-checkbox/radio-set (input)
  ((kind              :reader kind              :initarg :kind)
   (name              :reader name              :initarg :name)
   (value-label-alist :reader value-label-alist :initarg :value-label-alist)
   (checked           :reader checked           :initarg :checked))
  (:default-initargs :name (error 'slot-uninitialized :class 'input-checkbox/radio-set
                                                      :slot 'name)
                     :value-label-alist (error 'slot-uninitialized :class 'input-checkbox/radio-set
                                                                   :slot 'value-label-alist)
                     :checked nil))

(defclass input-radio-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "radio")))

(defclass input-checkbox-set (input-checkbox/radio-set)
  ((kind :reader kind :initform "checkbox")))

(defmethod display ((input-set input-checkbox/radio-set) &key
                    id css-class name value-label-alist
                    (checked nil checked-s) (readonly nil readonly-s) (disabled nil disabled-s))
  (with-html
    (:ul :id (or id (id input-set))
         :class (or css-class (css-class input-set))
         (iter (for (value . label) in (or value-label-alist (value-label-alist input-set)))
               (htm (:li (:input :type (string-downcase (kind input-set))
                                 :name (string-downcase (or name (name input-set)))
                                 :value (lisp->html value)
                                 :checked (if checked-s checked (equal value (checked input-set)))
                                 :readonly (if readonly-s readonly (readonly input-set))
                                 :disabled (if disabled-s disabled (disabled input-set))
                                 (display label))))))))

(defun input-checkbox-set (name value-label-alist &rest initargs
                                                  &key id css-class disabled readonly checked)
  (declare (ignore id css-class disabled readonly checked))
  (display (apply #'make-instance 'input-checkbox-set :name name
                                                      :value-label-alist value-label-alist
                                                      initargs)))

(defun input-radio-set (name value-label-alist &rest initargs
                                               &key id css-class disabled readonly checked)
  (declare (ignore id css-class disabled readonly checked))
  (display (apply #'make-instance 'input-radio-set :name name
                                                   :value-label-alist value-label-alist
                                                   initargs)))



;;; ------------------------------------------------------------
;;; dropdown menus
;;; ------------------------------------------------------------

(defclass dropdown (form-element)
  ((name              :reader name              :initarg :name)
   (value-label-alist :reader value-label-alist :initarg :value-label-alist)
   (selected          :reader selected          :initarg :selected))
  (:default-initargs :name (error 'slot-uninitialized :class 'dropdown
                                                      :slot 'name)
                     :value-label-alist (error 'slot-uninitialized :class 'dropdown
                                                                   :slot 'value-label-alist)
                     :selected nil))

(defmethod display ((dropdown dropdown) &key id css-class
                                             (disabled nil disabled-s)
                                             name value-label-alist
                                             (selected nil selected-s))
  (with-html
    (:select :id (or id (id dropdown))
             :class (or css-class (css-class dropdown))
             :name (string-downcase (or name (name dropdown)))
             :disabled (if disabled-s disabled (disabled dropdown))
             (iter (for (value . label) in (or value-label-alist (value-label-alist dropdown)))
               (htm (:option :value (lisp->html value)
                             :selected (equal value
                                              (if selected-s selected (selected dropdown)))
                             (display label)))))))

(defun dropdown (name value-label-alist &rest initargs &key id css-class disabled selected)
  (declare (ignore id css-class disabled selected))
  (display (apply #'make-instance 'dropdown :name name
                                    :value-label-alist value-label-alist
                                    initargs)))



;;; ------------------------------------------------------------
;;; buttons
;;; ------------------------------------------------------------

(defclass button (form-element)
  ((kind     :reader kind     :initform "button")
   (body     :reader body     :initarg  :body)
   (name     :reader name     :initarg  :name)
   (value    :reader value    :initarg  :value))
  (:default-initargs :body (error 'slot-uninitialized :class 'button :slot 'body)
                     :name nil
                     :value nil))

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

(defun button (body &rest initargs &key id css-class disabled name value)
  (declare (ignore id css-class disabled name value))
  (display (apply #'make-instance 'button :body body initargs)))

(defun submit (body &rest initargs &key id css-class disabled name value)
  (declare (ignore id css-class disabled name value))
  (display (apply #'make-instance 'submit :body body initargs)))



;;; ------------------------------------------------------------
;;; label
;;; ------------------------------------------------------------

(defun label (name body &key id css-class)
  (with-html
    (:label :id id
            :class css-class
            :for (string-downcase name)
            (display body))))