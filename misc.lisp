(in-package :bricks)



;;; ------------------------------------------------------------
;;; MULTISTATE ANCHORS
;;; ------------------------------------------------------------

(defclass multistate-anchor (widget)
  ((href  :reader href  :initarg :href)
   (body  :reader body  :initarg :body)
   (state :reader state :initarg :state)))

(defmethod display ((multistate-anchor multistate-anchor) &key)
  (let ((state (state multistate-anchor)))
    (with-html
      (:a :id (id multistate-anchor)
        :class (css-class multistate-anchor)
        :href (getf (href multistate-anchor) state)
        (display (getf (body multistate-anchor) state))))))

(defun multistate-anchor (href body &key state)
  (display (make-instance 'multistate-anchor
                          :href href
                          :contant body
                          :state state)))




;;; ------------------------------------------------------------
;;; Messenger widget
;;; ------------------------------------------------------------

(defclass messenger (widget)
  ((messages   :accessor messages   :initarg :messages)
   (parameters :accessor parameters :initarg :parameters))
  (:default-initargs :id nil))

(defmethod display ((messenger messenger) &key)
  (flet ((get-message (param messages)
           (if-let (msg-plist (second (assoc (parameter-name (attributes param)) messages
                                             :test #'member :key #'ensure-list)))
             ;; if the name of the parameter is not found, don't print any messages
             (if-let (tail (member (error-type param) msg-plist))
               ;; Use member to extract message from plist instead of
               ;; getf, to be able to have nil as a value (the cadr of
               ;; tail may be nil) and not get the fallback
               (cadr tail)
               (string-downcase (error-type param)))
             nil)))
    (unless (every #'validp (parameters messenger))
      (with-html
        (:ul :id (id messenger)
          (mapc (lambda (p)
                  (unless (validp p)
                    (when-let (msg (get-message p (messages messenger)))
                      (htm (:li :class (css-class messenger)
                             (str msg))))))
                (parameters messenger)))))))

(defun messenger (messages parameters &rest instance-initargs)
  (display (apply #'make-instance 'messenger
                  :messages messages
                  :parameters parameters
                  instance-initargs)))



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

(defmethod display ((navbar navbar) &key id css-class spec test active)
  (let ((test-fn (or test (test navbar)))
        (active-tag (or active (active navbar))))
    (with-html
      (:div :id (or id (id navbar)) :class (or css-class (css-class navbar))
        (:ul (mapc (lambda (tuple)
                     (destructuring-bind (tag-name href label) tuple
                       (htm (:li (if (funcall test-fn tag-name active-tag)
                                     (htm (:span (str label)))
                                     (htm (:a :href href
                                            (str label))))))))
                   (or spec (spec navbar))))))))

(defun navbar (spec &rest initargs &key id css-class test active)
  (declare (ignore id css-class test active))
  (display (apply #'make-instance 'navbar :spec spec initargs)))



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

(defmethod display ((menu menu) &key id css-class spec disabled)
  (let ((effective-spec (or spec (spec menu)))
        (effective-disabled (or disabled (disabled menu))))
    (with-html
      (:div :id (or id (id menu)) :class (or css-class (css-class menu))
        (:ul
          (if (and effective-spec
                   (not (subsetp (mapcar #'first effective-spec) effective-disabled)))
              (mapc (lambda (pair)
                      (destructuring-bind (item-id body) pair
                        (unless (member item-id effective-disabled)
                          (htm (:li (display body))))))
                    (or spec (spec menu)))
              (htm (:li :class (css-disabled menu) "no available menu items "))))))))

(defun menu (spec &key id css-class disabled css-disabled)
  (let ((initargs (plist-collect-if #'identity
                                    (list :id id
                                          :css-class css-class
                                          :disabled disabled
                                          :css-disabled css-disabled)
                                    :on-values-p t)))
    (display (apply #'make-instance 'menu :spec spec initargs))))



;;; ------------------------------------------------------------
;;; TEXTBOXES
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
