(in-package :bricks)


;;; ------------------------------------------------------------
;;; Textboxes
;;; Like a normal input-text widget when it is not disabled
;;; Changes to a span tag when disabled
;;; ------------------------------------------------------------

(defclass textbox (input-text)
  ((href      :accessor href      :initarg :href)
   (format-fn :accessor format-fn :initarg :format-fn))
  (:default-initargs :disabled nil
                     :name (error 'slot-uninitialized :class 'textbox :slot 'name)
                     :value nil
                     :readonly nil
                     :password nil
                     :href nil
                     :format-fn nil))

(defmethod display ((textbox textbox) &key id css-class value name href
                                           (format-fn nil format-fn-s)
                                           (readonly nil readonly-s)
                                           (disabled nil disabled-s)
                                           (password nil password-s))
  (let ((disabled-p (if disabled-s disabled (disabled textbox)))
        (password-p (if password-s password (password textbox)))
        (format-fn (or (if format-fn-s format-fn (format-fn textbox))
                       #'identity)))
    (if disabled-p
        (if (or href (href textbox))
            (with-html
              (:a :id (or id (id textbox))
                :class (or css-class (css-class textbox))
                :href (or href (href textbox))
                (str (lisp->html (funcall format-fn
                                          (or value (value textbox) :null))))))
            (with-html
              (:span :id (or id (id textbox))
                :class (or css-class (css-class textbox))
                (str (lisp->html (funcall format-fn
                                          (or value (value textbox) :null)))))))
        (with-html
          (:input :id (or id (id textbox))
            :class (or css-class (css-class textbox))
            :type (if password-p "password" "text")
            :name (string-downcase (or name (name textbox)))
            :value (lisp->html (funcall format-fn
                                        (or value (value textbox) :null)))
            :readonly (if readonly-s readonly (readonly textbox)))))))

(defun textbox (name &key id css-class disabled value readonly password href format-fn)
  (display (make-instance 'textbox
                          :id id
                          :css-class css-class
                          :disabled disabled
                          :name name
                          :value value
                          :readonly readonly
                          :password password
                          :href href
                          :format-fn format-fn)))
