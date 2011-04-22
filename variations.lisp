(in-package :bricks)


;;; ------------------------------------------------------------
;;; Textboxes
;;; Like a normal input-text widget when it is not disabled
;;; Changes to a span tag when disabled
;;; ------------------------------------------------------------

(defclass textbox (input-text)
  ())

(defmethod display ((textbox textbox) &key
                    id style value name
                    (readonly nil readonly-s) (disabled nil disabled-s) (password nil password-s))

  (let ((disabled-p (if disabled-s disabled (disabled textbox)))
        (password-p (if password-s password (password textbox))))
    (if disabled-p
        (with-html
          (:span :id (or id (id textbox))
                 :class (or style (style textbox))
                 (str (lisp->html (or value (value textbox) :null)))))
        (with-html
          (:input :id (or id (id textbox))
                  :class (or style (style textbox))
                  :type (if password-p "password" "text")
                  :name (string-downcase (or name (name textbox)))
                  :value (lisp->html (or value (value textbox) :null))
                  :readonly (if readonly-s readonly (readonly textbox)))))))

(defun textbox (name &rest instance-initargs)
  (display (apply #'make-instance 'textbox
                  :name name
                  instance-initargs)))
