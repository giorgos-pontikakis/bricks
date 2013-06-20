(in-package :bricks)



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form (widget)
  ((action  :reader action  :initarg :action)
   (reqtype :reader reqtype :initarg :reqtype)
   (hidden  :reader hidden  :initarg :hidden)
   (body    :reader body    :initarg :body))
  (:default-initargs :action (error 'slot-uninitialized :class 'form :slot 'action)
                     :body (error 'slot-uninitialized :class 'form :slot 'body)
                     :hidden nil
                     :reqtype "GET"))

(defmethod display ((form form) &key)
  (with-html
    (:form :action (action form)
           :method (reqtype form)
           :id (id form)
           :css-class (css-class form)
      (plist-mapc (lambda (key val)
                    (when val
                      (htm
                       (:input :type "hidden"
                         :name (string-downcase key)
                         :value (lisp->html val)))))
                  (hidden form))
      (display (body form)))))



;;; ------------------------------------------------------------
;;; Form elements
;;; ------------------------------------------------------------

(defclass form-element (widget)
  ((disabled :reader disabled :initarg :disabled))
  (:default-initargs :disabled nil))

(defclass input (form-element)
  ((readonly :reader readonly :initarg :readonly))
  (:default-initargs :readonly nil))



;;; ------------------------------------------------------------
;;; text input boxes
;;; ------------------------------------------------------------

(defclass input-text (input)
  ((name     :reader name     :initarg :name)
   (value    :reader value    :initarg :value)
   (password :reader password :initarg :password))
  (:default-initargs :name (error 'slot-uninitialized :class 'input-text :slot 'name)
                     :value :null
                     :password nil))

(defmethod display ((input-text input-text) &key)
  (with-html
    (:input :id (id input-text)
            :class (css-class input-text)
            :type (if (password input-text) "password" "text")
            :name (string-downcase (name input-text))
            :value (lisp->html (or (value input-text)
                                   :null)) ; in case value is supplied and is NIL
            :readonly (readonly input-text)
            :disabled (disabled input-text))))



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

(defmethod display ((checkable input-checkbox/radio) &key)
  (with-html
    (:input :id (id checkable)
            :class (css-class checkable)
            :type (kind checkable)
            :name (string-downcase (name checkable))
            :value (lisp->html (value checkable))
            :readonly (readonly checkable)
            :disabled (disabled checkable)
            :checked (checked checkable)
            (display (body checkable)))))



;;; ------------------------------------------------------------
;;; radio/checkbox input sets
;;; ------------------------------------------------------------

(defclass input-checkbox/radio-set (input)
  ((name              :reader name              :initarg :name)
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

(defmethod display ((input-set input-checkbox/radio-set) &key)
  (with-html
    (:ul :id (id input-set)
         :class (css-class input-set)
         (mapc (lambda (pair)
                 (destructuring-bind (value . label) pair
                   (htm (:li (:input :type (string-downcase (kind input-set))
                                     :name (string-downcase (name input-set))
                                     :value (lisp->html value)
                                     :checked (equal value (checked input-set))
                                     :readonly (readonly input-set)
                                     :disabled (disabled input-set)
                                     (display label))))))
               (value-label-alist input-set)))))



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

(defmethod display ((dropdown dropdown) &key)
  (with-html
    (:select :id (id dropdown)
             :class (css-class dropdown)
             :name (string-downcase (name dropdown))
             :disabled (disabled dropdown)
             (mapc (lambda (pair)
                     (destructuring-bind (value . label) pair
                       (htm (:option :value (lisp->html value)
                                     :selected (equal value
                                                      (selected dropdown))
                                     (display label)))))
                   (value-label-alist dropdown)))))



;;; ------------------------------------------------------------
;;; buttons
;;; ------------------------------------------------------------

(defclass button (form-element)
  ((kind  :reader kind  :initform "button")
   (body  :reader body  :initarg  :body)
   (name  :reader name  :initarg  :name)
   (value :reader value :initarg  :value))
  (:default-initargs :body (error 'slot-uninitialized :class 'button :slot 'body)
                     :name nil
                     :value nil))

(defclass submit (button)
  ((kind :reader kind :initform "submit")))

(defmethod display ((button button) &key)
  (with-html
    (:button :id (id button)
             :class (css-class button)
             :type (kind button)
             :name (if-let (n (name button))
                     (string-downcase n)
                     nil)
             :value (if-let (v (value button))
                      (lisp->html v)
                      nil)
             :disabled (disabled button)
             (display (body button)))))
