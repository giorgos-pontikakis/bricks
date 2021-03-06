(in-package :bricks)


;;; ----------------------------------------------------------------------
;;; NAVBARS
;;;
;;; A navbar is a unordered list of anchors. One of them may be
;;; active, i.e. its class is active-style instead of
;;; inactive-style. The idea is to represent the link to the page we
;;; are currently viewing with a separate style.
;;; ----------------------------------------------------------------------

(defclass navbar (widget)
  ((spec   :reader spec   :initarg :spec)
   (test   :reader test   :initarg :test)
   (active :reader active :initarg :active))
  (:default-initargs :spec (error 'navbar :class 'navbar :slot 'spec)
                     :test #'eql
                     :active nil))

(defmethod display ((navbar navbar) &key)
  (let ((test-fn (test navbar))
        (active-tag (active navbar)))
    (with-html
      (:div :id (id navbar) :class (css-class navbar)
        (:ul (mapc (lambda (tuple)
                     (destructuring-bind (tag-name href label) tuple
                       (htm (:li (if (funcall test-fn tag-name active-tag)
                                     (htm (:span (str label)))
                                     (htm (:a :href href
                                            (str label))))))))
                   (spec navbar)))))))



;;; ----------------------------------------------------------------------
;;; MENUS
;;;
;;; A menu is an unordered list of widgets. Some may be disabled.
;;; ----------------------------------------------------------------------

(defclass menu (widget)
  ((spec         :reader   spec         :initarg :spec)
   (disabled     :reader   disabled     :initarg :disabled)
   (css-disabled :accessor css-disabled :initarg :css-disabled))
  (:default-initargs :spec (error 'navbar :class 'navbar :slot 'spec)
                     :disabled '() :css-disabled nil))

(defmethod display ((menu menu) &key)
  (let ((effective-spec (spec menu))
        (effective-disabled (disabled menu)))
    (with-html
      (:div :id (id menu) :class (css-class menu)
            (:ul
             (if (and effective-spec
                      (not (subsetp (mapcar #'first effective-spec) effective-disabled)))
                 (mapc (lambda (pair)
                         (destructuring-bind (item-id body) pair
                           (unless (member item-id effective-disabled)
                             (htm (:li (display body))))))
                       (spec menu))
                 (htm (:li :class (css-disabled menu) "no available menu items "))))))))



;;; ------------------------------------------------------------
;;; TEXTBOXES
;;; Like a normal input-text widget when it is not disabled
;;; Changes to a span tag when disabled
;;; ------------------------------------------------------------

(defclass textbox (input-text)
  ((href      :accessor href      :initarg :href)
   (format-fn :accessor format-fn :initarg :format-fn))
  (:default-initargs :href nil
                     :format-fn nil))

(defmethod display ((textbox textbox) &key)
  (let ((disabled-p (disabled textbox))
        (password-p (password textbox))
        (format-fn (or (format-fn textbox) #'identity)))
    (if disabled-p
        (if (href textbox)
            (with-html
              (:a :id (id textbox)
                  :class (css-class textbox)
                  :href (href textbox)
                  (str (lisp->html (funcall format-fn
                                            (value textbox))))))
            (with-html
              (:span :id (id textbox)
                     :class (css-class textbox)
                     (str (lisp->html (funcall format-fn
                                               (value textbox)))))))
        (with-html
          (:input :id (id textbox)
                  :class (css-class textbox)
                  :type (if password-p "password" "text")
                  :name (string-downcase (name textbox))
                  :value (lisp->html (funcall format-fn
                                              (value textbox)))
                  :readonly (readonly textbox))))))
