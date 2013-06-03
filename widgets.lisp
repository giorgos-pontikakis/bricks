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
           (mapcar #'slot-definition-name
                   (mapcan #'(lambda (class)
                               (finalize-inheritance class)
                               (class-slots class))
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



;;; ----------------------------------------------------------------------
;;; attributes metaclass
;;; ----------------------------------------------------------------------

(defclass attributes-class (standard-class)
  ())

(defmethod validate-superclass ((class attributes-class) (superclass standard-class))
  (declare (ignore class superclass))
  t)

(defclass attributes-slot-mixin ()
  ((required :accessor required :initarg :required))
  (:default-initargs :required nil :default nil))


(defclass attributes-direct-slot-definition (standard-direct-slot-definition
                                             attributes-slot-mixin)
  ())

(defclass attributes-effective-slot-definition (standard-effective-slot-definition
                                                attributes-slot-mixin)
  ())

(defmethod direct-slot-definition-class ((class attributes-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'attributes-direct-slot-definition))

(defmethod effective-slot-definition-class ((class attributes-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'attributes-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class attributes-class)
                                              slot-name
                                              direct-slot-definitions)
  (let ((effective-slot (call-next-method))
        (direct-slot (find slot-name direct-slot-definitions :key #'slot-definition-name))
        (attributes (mapcar #'slot-definition-name
                            (class-slots (ensure-finalized (find-class 'attributes-slot-mixin))))))
    (dolist (attr attributes)
      (setf (slot-value effective-slot attr)
            (slot-value direct-slot attr)))
    effective-slot))

(defgeneric find-slot (class slot))

(defmethod find-slot (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))


(defgeneric attribute (class slot attribute))

(defmethod attribute ((class attributes-class) slot-name attribute-name)
  (let ((slot-obj (find-slot (ensure-finalized class) slot-name)))
    (slot-value slot-obj attribute-name)))

(defmethod attribute ((class symbol) slot-name attribute-name)
  (attribute (find-class class) slot-name attribute-name))




(defclass brick ()
  ((id        :accessor id        :initarg :id        :required t)
   (css-class :accessor css-class :initarg :css-class)
   (asdf      :accessor asdf      :initarg :asdf      :required t))
  (:metaclass attributes-class))

;; (defclass brick-form (brick)
;;   ((reqtype :accessor reqtype :initarg :reqtype)
;;    (hidden  :accessor hidden  :initarg :hidden)
;;    (body    :accessor body    :initarg :body    :required t)
;;    (action  :accessor action  :initarg :action  :required t))
;;   (:default-initargs :reqtype "GET"))

;; (defmacro defbrick (class (&rest super-classes) slots &body body)
;;   (flet ((super-slots (super-classes)
;;            (mapcar #'slot-definition-name
;;                    (mapcan #'(lambda (class)
;;                                (finalize-inheritance class)
;;                                (class-slots class))
;;                            (mapcar #'find-class super-classes))))
;;          (wrap-display-arg (arg)
;;            `(,arg nil ,(gensym)))
;;          (display-bindings (display-arglist)
;;            (mapcar (lambda (composite-arg)
;;                      (destructuring-bind (name default supplied-p) composite-arg
;;                        (declare (ignore default))
;;                        `(,name (if ,supplied-p
;;                                    ,name
;;                                    (,name ,class)))))
;;                    display-arglist)))

;;     (let* ((super-slots (super-slots super-classes))
;;            direct-slots
;;            required-slots
;;            optional-slots
;;            defaults-arglist
;;            (instance-arglist (loop for s in super-slots
;;                                    collect (make-keyword s)
;;                                    collect s)))
;;       (dolist (slot-spec (reverse slots))
;;         (destructuring-bind (slot &key default requiredp) (ensure-list slot-spec)
;;           (let ((slot-keyword (make-keyword slot)))
;;             ;; all class slots
;;             (push `(,slot :reader ,slot :initarg ,slot-keyword) direct-slots)
;;             ;; make-instance arglist
;;             (setf instance-arglist
;;                   (nconc instance-arglist
;;                          `(,slot-keyword ,slot)))
;;             ;; class slots
;;             (cond ((and requiredp default)
;;                    (error (format nil
;;                                   "Either requiredp or default must be specified in DEFWIDGET ~A, slot ~A"
;;                                   class slot)))
;;                   (requiredp
;;                    (setf defaults-arglist
;;                          (nconc defaults-arglist
;;                                 `(,slot-keyword
;;                                   (error 'slot-uninitialized :class ',class :slot ',slot))))
;;                    (push slot required-slots))
;;                   (default
;;                    (setf defaults-arglist
;;                          (nconc `(,slot-keyword ,default)
;;                                 defaults-arglist))
;;                    (push slot optional-slots))
;;                   (t
;;                    (push slot optional-slots))))))
;;       (let ((display-arglist (mapcar #'wrap-display-arg
;;                                      `(,@required-slots
;;                                        ,@optional-slots
;;                                        ,@super-slots))))
;;         `(eval-when (:compile-toplevel :load-toplevel :execute)
;;            (defclass ,class (,@super-classes)
;;              ,direct-slots
;;              (:default-initargs ,@defaults-arglist))
;;            (defmethod display ((,class ,class) &key ,@display-arglist)
;;              (let ,(display-bindings display-arglist)
;;                ,@body))
;;            (defun ,class (,@required-slots &key ,@optional-slots ,@(super-slots super-classes))
;;              (display (make-instance ',class
;;                                      ,@instance-arglist))))))))
