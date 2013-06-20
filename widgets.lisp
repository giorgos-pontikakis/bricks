(in-package :bricks)



;;; ----------------------------------------------------------------------
;;; widgets class
;;; ----------------------------------------------------------------------

(defclass widget ()
  ((id        :reader id        :initarg :id)
   (css-class :reader css-class :initarg :css-class))
  (:default-initargs :id nil
                     :css-class nil))



;;; ----------------------------------------------------------------------
;;; display generic function and basic methods
;;; ----------------------------------------------------------------------

(defgeneric display (widget &rest args)
  (:documentation "Display a widget as html."))

(defmethod display ((widget function) &rest args)
  (apply widget args))

(defmethod display ((widget list) &rest args)
  (mapc (lambda (item)
          (apply #'display item args))
        widget))

(defmethod display ((widget t) &rest args)
  (declare (ignore args)) ;; fallback case - ignore arguments and rely on lisp->html
  (with-html
    (str (lisp->html widget))))
