(in-package :bricks)



;;; ------------------------------------------------------------
;;; MULTISTATE ANCHOR
;;; ------------------------------------------------------------

(defclass multistate-anchor (widget)
  ((href  :accessor href  :initarg :href)
   (body  :accessor body  :initarg :body)
   (state :accessor state :initarg :state)))

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
  (with-html
    (:div :id (or id (id navbar)) :class (or css-class (css-class navbar))
          (:ul
           (iter (with test-fn = (or test (test navbar)))
                 (with active-item = (or active (active navbar)))
                 (for (page-name href label) in (or spec (spec navbar)))
                 (htm (:li (if (funcall test-fn page-name active-item)
                               (htm (:span (str label)))
                               (htm (:a :href href
                                        (str label)))))))))))

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
  (with-html
    (:div :id (or id (id menu)) :class (or css-class (css-class menu))
          (:ul
           (if (and (or spec (spec menu)) ; empty spec
                    (not (subsetp (mapcar #'first spec) disabled))) ; all items disabled
               (iter (for (item-id body) in (or spec (spec menu)))
                 (unless (member item-id (or disabled (disabled menu)))
                     (htm (:li (display body)))))
               (htm (:li :class (css-disabled menu) "no available menu items ")))))))

(defun menu (spec &key id css-class disabled css-disabled)
  (let ((initargs (plist-collect-if #'identity
                                    (list :id id
                                          :css-class css-class
                                          :disabled disabled
                                          :css-disabled css-disabled)
                                    :on-values-p t)))
    (display (apply #'make-instance 'menu :spec spec initargs))))
