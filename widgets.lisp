(in-package :bricks)



;;; For each widget, we provide the class, the display method and a
;;; rendering function with the same name as the class.
;;;
;;; The display method provides the slot names as keys, so that we can
;;; override values of the object by calling display with different
;;; keyword arguments.
;;;
;;; Each class provides default initarg values for every slot which is
;;; conceptually optional and appears as a keyword in the rendering
;;; function signature.
;;;
;;; For every slot which is necessary for the widget to render, we
;;; need a required argument in the rendering function signature. We
;;; do not provide a default initarg so that we get an error if we
;;; fail to specify a value via make-instance or display.


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

(defmethod display ((widget t) &key)
  (with-html
    (str (lisp->html widget))))



;;; ----------------------------------------------------------------------
;;; widget macro
;;; ----------------------------------------------------------------------

(defmacro defwidget (class (&rest super-classes) slots &body body)
  (flet ((super-slots (super-classes)
           (mapcar #'closer-mop:slot-definition-name
                   (mapcan #'(lambda (class)
                               (closer-mop:finalize-inheritance class)
                               (closer-mop:class-slots class))
                           (mapcar #'find-class super-classes))))
         (wrap-display-arg (arg)
           `(,arg nil ,(gensym)))
         (effective-args (display-arglist)
           (mapcar (lambda (composite-arg)
                     (destructuring-bind (name default supplied-p) composite-arg
                       (declare (ignore default))
                       `(,name (if ,supplied-p
                                   ,name
                                   (,name ,class)))))
                   display-arglist)))
    (let* ((super-slots (super-slots super-classes))
           direct-slots
           required-slots
           optional-slots
           defaults-arglist
           (instance-arglist (loop for s in super-slots
                                   collect (make-keyword s)
                                   collect s)))
      (dolist (slot-spec (reverse slots))
        (destructuring-bind (slot &key default requiredp) (ensure-list slot-spec)
          (let ((slot-keyword (make-keyword slot)))
            ;; all class slots
            (push `(,slot :reader ,slot :initarg ,slot-keyword) direct-slots)
            ;; make-instance arglist
            (setf instance-arglist
                  (nconc instance-arglist
                         `(,slot-keyword ,slot)))
            ;; class slots
            (cond ((and requiredp default)
                   (error (format nil
                                  "Either requiredp or default must be specified in DEFWIDGET ~A, slot ~A"
                                  class slot)))
                  (requiredp
                   (setf defaults-arglist
                         (nconc defaults-arglist
                                `(,slot-keyword
                                  (error 'slot-uninitialized :class ',class :slot ',slot))))
                   (push slot required-slots))
                  (default
                   (setf defaults-arglist
                         (nconc `(,slot-keyword ,default)
                                defaults-arglist))
                   (push slot optional-slots))
                  (t
                   (push slot optional-slots))))))
      (let ((display-arglist (mapcar #'wrap-display-arg
                                     `(,@required-slots
                                       ,@optional-slots
                                       ,@super-slots))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defclass ,class (,@super-classes)
             ,direct-slots
             (:default-initargs ,@defaults-arglist))
           (defmethod display ((,class ,class) &key ,@display-arglist)
             (let ,(effective-args display-arglist)
               ,@body))
           (defun ,class (,@required-slots &key ,@optional-slots ,@(super-slots super-classes))
             (display (make-instance ',class
                                     ,@instance-arglist))))))))
