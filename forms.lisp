(in-package :bricks)


;;; ------------------------------------------------------------
;;; Records
;;; ------------------------------------------------------------

(defgeneric get-record (widget)
  (:documentation "Returs a record of the widget"))



;;; ------------------------------------------------------------
;;; Data Forms
;;; ------------------------------------------------------------

(defclass crud-form (widget)
  ((op           :accessor op           :initarg :op)
   (key          :accessor key          :initarg :key)
   (record       :accessor record       :initarg :record)
   (record-class :accessor record-class :initarg :record-class))
  (:default-initargs :key nil))

(defmethod initialize-instance :after ((form crud-form) &key)
  (when (and (eql (op form) :create)
             (key form))
    (error "Contradiction in crud-form initialization. Slot OP is :create and slot KEY is not null"))
  (unless (slot-boundp form 'record)
    (setf (slot-value form 'record) (if (key form)
                                        (get-record form)
                                        (make-record (record-class form))))))

(defmethod display :before ((form crud-form) &key payload)
  (when (member (op form) '(:create :update))
    (setf (record form) (update-record (record form) payload))))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defclass form (widget)
  ((action  :reader action  :initarg :action)
   (reqtype :reader reqtype :initarg :reqtype)
   (hidden  :reader hidden  :initarg :hidden)
   (body    :reader body    :initarg :body))
  (:default-initargs :hidden nil :reqtype "GET"))

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

;; (defun form (action body &key id css-class reqtype hidden)
;;   (display (make-instance 'form
;;                           :id id
;;                           :css-class css-class
;;                           :reqtype reqtype
;;                           :action action
;;                           :body body
;;                           :hidden hidden)))



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
                     :value nil
                     :password nil))

(defmethod display ((input-text input-text) &key)
  (with-html
    (:input :id (id input-text)
            :class (css-class input-text)
            :type (if (password input-text) "password" "text")
            :name (string-downcase (name input-text))
            :value (lisp->html (or (value input-text) :null))
            :readonly (readonly input-text)
            :disabled (disabled input-text))))

;; (defun input-text (name &rest initargs &key id css-class disabled readonly value password)
;;   (declare (ignore id css-class disabled readonly value password))
;;   (display (apply #'make-instance 'input-text :name name initargs)))





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

;; (defun input-radio (name value body &rest initargs &key id css-class disabled readonly checked)
;;   (declare (ignore id css-class disabled readonly checked))
;;   (display (apply #'make-instance 'input-radio :name name
;;                                                :value value
;;                                                :body body
;;                                                initargs)))

;; (defun input-checkbox (name value body &rest initargs &key id css-class disabled readonly checked)
;;   (declare (ignore id css-class disabled readonly checked))
;;   (display (apply #'make-instance 'input-checkbox :name name
;;                                                   :value value
;;                                                   :body body
;;                                                   initargs)))


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

;; (defun input-checkbox-set (name value-label-alist &rest initargs
;;                                                   &key id css-class disabled readonly checked)
;;   (declare (ignore id css-class disabled readonly checked))
;;   (display (apply #'make-instance 'input-checkbox-set :name name
;;                                                       :value-label-alist value-label-alist
;;                                                       initargs)))

;; (defun input-radio-set (name value-label-alist &rest initargs
;;                                                &key id css-class disabled readonly checked)
;;   (declare (ignore id css-class disabled readonly checked))
;;   (display (apply #'make-instance 'input-radio-set :name name
;;                                                    :value-label-alist value-label-alist
;;                                                    initargs)))



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

;; (defun dropdown (name value-label-alist &rest initargs &key id css-class disabled selected)
;;   (declare (ignore id css-class disabled selected))
;;   (display (apply #'make-instance 'dropdown :name name
;;                                             :value-label-alist value-label-alist
;;                                             initargs)))



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

;; (defun button (body &rest initargs &key id css-class disabled name value)
;;   (declare (ignore id css-class disabled name value))
;;   (display (apply #'make-instance 'button :body body initargs)))

;; (defun submit (body &rest initargs &key id css-class disabled name value)
;;   (declare (ignore id css-class disabled name value))
;;   (display (apply #'make-instance 'submit :body body initargs)))



;;; ------------------------------------------------------------
;;; label
;;; ------------------------------------------------------------

;; (defun label (name body &key id css-class)
;;   (with-html
;;     (:label :id id
;;       :class css-class
;;       :for (string-downcase name)
;;       (display body))))
