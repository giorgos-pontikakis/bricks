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
  ((spec             :reader spec             :initarg :spec)
   (test             :reader test             :initarg :test)
   (active-page-name :reader active-page-name :initarg :active-page-name))
  (:default-initargs :test #'eql))

(defmethod display ((navbar navbar) &key active-page-name test)
  (with-html
    (:div :id (id navbar) :class (css-class navbar)
          (:ul
           (iter (with test-fn = (or test (test navbar)))
                 (with active = (or active-page-name (active-page-name navbar)))
                 (for (page-name href label) in (spec navbar))
                 (htm (:li (if (funcall test-fn page-name active)
                               (htm (:span (str label)))
                               (htm (:a :href href
                                        (str label)))))))))))

(defun navbar (spec &rest instance-initargs)
  (display (apply #'make-instance 'navbar
                  :spec spec
                  instance-initargs)))



;;; ----------------------------------------------------------------------
;;; MENUS
;;;
;;; A menu is an unordered list of anchors. Some may be disabled.
;;; ----------------------------------------------------------------------

(defclass menu (widget)
  ((spec     :reader spec     :initarg :spec)
   (disabled :reader disabled :initarg :disabled))
  (:default-initargs :disabled '()))

(defmethod display ((menu menu) &key spec disabled)
  (with-html
    (:div :id (id menu) :class (css-class menu)
          (:ul
           (iter (for (action-id href label) in (or spec (spec menu)))
                 (unless (or (member action-id (or disabled (disabled menu)))
                             (null href))
                   (htm (:li (:a :href href
                                 :class (string-downcase action-id)
                                 (str label))))))))))

(defun menu (spec &rest instance-initargs)
  (display (apply #'make-instance 'menu :spec spec instance-initargs)))



;;; ------------------------------------------------------------
;;; MESSENGER
;;; ------------------------------------------------------------

(defclass messenger (widget)
  ((messages   :accessor messages   :initarg :messages)
   (parameters :accessor parameters :initarg :parameters))
  (:default-initargs :id nil))

(defmethod display ((messenger messenger) &key)
  (flet ((get-message (param messages)
           (if-let (msg-plist (assoc (name param) messages))
             ;; if the name of the parameter is not found, don't print any messages
             (if-let (tail (member (error-type param) (second msg-plist)))
               ;; Use member to extract message from plist instead of
               ;; getf, to be able to have nil as a value (the cadr of
               ;; tail may be nil) and not get the fallback
               (cadr tail)
               (string-downcase (error-type param)))
             nil)))
    (unless (every #'validp (parameters messenger))
      (with-html
        (:ul :id (id messenger)
             (iter (for p in (parameters messenger))
                   (unless (validp p)
                     (when-let (msg (get-message p (messages messenger)))
                       (htm (:li :class (css-class messenger)
                                 (str msg)))))))))))

(defun messenger (messages parameters &rest instance-initargs)
  (display (apply #'make-instance 'messenger
                  :messages messages
                  :parameters parameters
                  instance-initargs)))