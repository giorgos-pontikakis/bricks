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
  ((spec             :accessor spec             :initarg :spec)
   (active-page-name :accessor active-page-name :initarg :active-page-name)))

(defmethod display ((navbar navbar))
  (with-html
    (:div :id (id navbar) :class (style navbar)
          (:ul
           (iter (for (page-name href label) in (spec navbar))
                 (htm (:li (if (eql page-name active-page-name)
                               (htm (:span (str label)))
                               (htm (:a :href href
                                        (str label)))))))))))

(defun lazy-navbar (spec &key active-page-name)
  (make-instance 'navbar :spec spec :active-page-name active-page-name))

(defun navbar (spec &key active-page-name)
  (display (lazy-navbar spec :active-page-name active-page-name)))




;;; ----------------------------------------------------------------------
;;; MENUS
;;;
;;; A menu is an unordered list of anchors. Some may be disabled.
;;; ----------------------------------------------------------------------

(defclass menu (widget)
  ((spec     :reader spec     :initarg :spec)
   (disabled :reader disabled :initarg :disabled)))

(defmethod display ((menu menu) &key disabled)
  (with-html
    (:div :id (id menu) :class (style menu)
          (:ul
           (iter (for (action-id href label) in (spec menu))
                 (unless (or (member action-id (or disabled (disabled menu)))
                             (null href))
                   (htm (:li (:a :href href
                                 :class (string-downcase action-id)
                                 (str label)))))))
          (:div :class "clear"))))



;;; ------------------------------------------------------------
;;; MESSENGER
;;; ------------------------------------------------------------

(defclass messenger (widget)
  ((messages :accessor messages :initarg :messages))
  (:default-initargs :id nil))

(defmethod display ((messenger messenger) &key params)
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
    (unless (every #'validp params)
      (with-html
        (:ul :id (id messenger)
             (iter (for p in params)
                   (unless (validp p)
                     (when-let (msg (get-message p (messages messenger)))
                       (htm (:li :class (style messenger)
                                 (str msg)))))))))))
