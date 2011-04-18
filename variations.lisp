(in-package :bricks)


;;; ------------------------------------------------------------
;;; Textboxes
;;; Like a normal input-text widget when it is not disabled
;;; Changes to a span tag when disabled
;;; ------------------------------------------------------------

(defclass textbox (input-text)
  ())

(defmethod display ((textbox textbox) &key id style value readonly disabled password)
  (if (or disabled (disabled textbox))
      (with-html
        (:span :id (or id (id textbox) (string-downcase (name textbox)))
               :class (or style (style textbox))
               (str (lisp->html (or value (value textbox) :null)))))
      (with-html
        (:input :id (or id (id textbox) (string-downcase (name textbox)))
                :class (or style (style textbox))
                :type (if (or password (password textbox)) "password" "text")
                :name (string-downcase (name textbox))
                :value (lisp->html (or value (value textbox) :null))
                :readonly (or readonly (readonly textbox))))))

(defun textbox (name &key id style readonly disabled password value)
  (display (make-instance 'textbox
                          :name name
                          :id id
                          :style style
                          :readonly readonly
                          :disabled disabled
                          :password password
                          :value value)))
