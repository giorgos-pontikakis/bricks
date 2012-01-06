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
  ((spec     :reader spec     :initarg :spec)
   (disabled :reader disabled :initarg :disabled))
  (:default-initargs :spec (error 'navbar :class 'navbar :slot 'spec)
                     :disabled '()))

(defmethod display ((menu menu) &key id css-class spec disabled)
  (with-html
    (:div :id (or id (id menu)) :class (or css-class (css-class menu))
          (:ul
           (iter (for (action-id body) in (or spec (spec menu)))
                 (unless (member action-id (or disabled (disabled menu)))
                   (htm (:li (display body)))))))))

(defun menu (spec &rest initargs &key id css-class disabled)
  (declare (ignore id css-class disabled))
  (display (apply #'make-instance 'menu :spec spec
                                        initargs)))
