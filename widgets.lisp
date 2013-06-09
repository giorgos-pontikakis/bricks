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

;; (defmacro defwidget (class (&rest super-classes) slots &body body)
;;   (flet ((super-slots (super-classes)
;;            (mapcar #'slot-definition-name
;;                    (mapcan #'(lambda (class)
;;                                (finalize-inheritance class)
;;                                (class-slots class))
;;                            (mapcar #'find-class super-classes))))
;;          (wrap-display-arg (arg)
;;            `(,arg nil ,(gensym)))
;;          (effective-args (display-arglist)
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
;;              (let ,(effective-args display-arglist)
;;                ,@body))
;;            (defun ,class (,@required-slots &key ,@optional-slots ,@(super-slots super-classes))
;;              (display (make-instance ',class
;;                                      ,@instance-arglist))))))))



;; ;;; ----------------------------------------------------------------------
;; ;;; attributes metaclass
;; ;;; ----------------------------------------------------------------------

;; (defclass attributes-class (standard-class)
;;   ())

;; (defmethod validate-superclass ((class attributes-class) (superclass standard-class))
;;   (declare (ignore class superclass))
;;   t)

;; (defclass attributes-slot-mixin ()
;;   ((required :accessor required :initarg :required))
;;   (:default-initargs :required nil))


;; (defclass attributes-direct-slot-definition (standard-direct-slot-definition
;;                                              attributes-slot-mixin)
;;   ())

;; (defclass attributes-effective-slot-definition (standard-effective-slot-definition
;;                                                 attributes-slot-mixin)
;;   ())

;; (defmethod direct-slot-definition-class ((class attributes-class) &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'attributes-direct-slot-definition))

;; (defmethod effective-slot-definition-class ((class attributes-class) &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'attributes-effective-slot-definition))

;; (defmethod compute-effective-slot-definition ((class attributes-class)
;;                                               slot-name
;;                                               direct-slot-definitions)
;;   (let ((effective-slot (call-next-method))
;;         (direct-slot (find slot-name direct-slot-definitions :key #'slot-definition-name))
;;         (attributes (mapcar #'slot-definition-name
;;                             (class-slots (ensure-finalized (find-class 'attributes-slot-mixin))))))
;;     (dolist (attr attributes)
;;       (setf (slot-value effective-slot attr)
;;             (slot-value direct-slot attr)))
;;     effective-slot))



;; ;;; attribute

;; (defgeneric attribute (class slot attribute))

;; (defmethod attribute ((class standard-class) slot attribute-name)
;;   (declare (ignore class slot attribute-name))
;;   nil)

;; (defmethod attribute ((class attributes-class)
;;                       (slot attributes-effective-slot-definition)
;;                       attribute-name)
;;   (slot-value slot attribute-name))

;; (defmethod attribute ((class attributes-class)
;;                       (slot symbol)
;;                       attribute-name)
;;   (attribute class (find-slot class slot) attribute-name))

;; (defmethod attribute ((class symbol)
;;                       slot
;;                       attribute-name)
;;   (attribute (find-class class) slot attribute-name))



;; ;;; attribute-slots

;; (defmethod attribute-slots ((class standard-class) attribute-name)
;;   (loop for slot in (class-slots (ensure-finalized class))
;;         when (attribute class slot attribute-name)
;;           collect slot))

;; (defmethod attribute-slots ((class symbol) attribute-name)
;;   (attribute-slots (find-class class) attribute-name))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun display-arglist (class-name)
;;     (mapcar (lambda (slot)
;;               (let ((slot-name (slot-definition-name slot)))
;;                 `(,slot-name nil ,(gensym))))
;;             (class-slots (find-class class-name)))))


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun display-bindings (display-arglist class-name)
;;     (mapcar (lambda (list)
;;               (destructuring-bind (name default supplied-p) list
;;                 (declare (ignore default))
;;                 `(,name (if ,supplied-p
;;                             ,name
;;                             (,name ,class-name)))))
;;             display-arglist)))



;; ;;; utilities

;; (defun find-slot (class slot-name)
;;   (find slot-name (class-slots (ensure-finalized class)) :key #'slot-definition-name))

;; (defun required-slot-names (class)
;;   (mapcar #'slot-definition-name (attribute-slots class 'required)))


;; (defclass nonbrick ()
;;   ((id        :accessor id-of     :initarg :id)
;;    (css-class :accessor css-class :initarg :css-class)
;;    (asdf      :accessor asdf      :initarg :asdf)))

;; (defclass brick ()
;;   ((id        :accessor id-of     :initarg :id        :required t :accessor id)
;;    (css-class :accessor css-class :initarg :css-class))
;;   (:metaclass attributes-class))

;; (defclass brick-kid (brick)
;;   ((id        :accessor id-of     :initarg :id        :required t :accessor id)
;;    (css-class :accessor css-class :initarg :css-class)
;;    (asdf      :accessor asdf      :initarg :asdf      :required t))
;;   (:metaclass attributes-class))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defmacro defbrick (class-name (&rest direct-superclasses) (&rest slots) &body body)
;;     (let ((direct-slots nil)
;;           (options (list '(:metaclass attributes-class))))
;;       ;; direct slots
;;       (dolist (slot slots)
;;         (setf slot (ensure-list slot))
;;         (unless (getf (cdr slot) :accessor)
;;           (nconc slot (list :accessor (first slot))))
;;         (unless (getf (cdr slot) :initarg)
;;           (nconc slot (list :initarg (make-keyword (first slot)))))
;;         (push slot direct-slots))
;;       ;; default-initargs
;;       (when (and (listp (car body))
;;                  (eql (caar body) :default-initargs))
;;         (push (pop body) options))
;;       ;; expansion
;;       `(progn
;;          (defclass ,class-name (,@direct-superclasses)
;;            ,direct-slots
;;            ,@options)
;;          (ensure-finalized (find-class ',class-name))))))

;; (defmacro defbrick* (class-name &body body)
;;   (let* ((display-arglist (display-arglist class-name))
;;          (display-bindings (display-bindings display-arglist class-name)))
;;     `(defmethod display ((,class-name ,class-name) &key ,@display-arglist)
;;        (let ,display-bindings
;;          ,@body))))


;; (defbrick brick-form (brick-kid)
;;     ((reqtype)
;;      hidden
;;      (body :required t)
;;      (action :required t))
;;   (:default-initargs :reqtype "GET")
;;   (with-html
;;     (:form :action action
;;            :method reqtype
;;            :id id
;;            :css-class css-class
;;            (plist-mapc (lambda (key val)
;;                          (when val
;;                            (htm
;;                             (:input :type "hidden"
;;                                     :name (string-downcase key)
;;                                     :value (lisp->html val)))))
;;                        hidden)
;;            (display body))))


;; (defclass brick-form ()
;;   ((reqtype :accessor reqtype :initarg :reqtype)
;;    (hidden  :accessor hidden  :initarg :hidden)
;;    (body    :accessor body    :initarg :body)
;;    (action  :accessor action  :initarg :action))
;;   (:default-initargs :reqtype "GET"))

;; (defmethod display ((form brick-form))
;;   (with-html
;;     (:form :action (action form)
;;            :method (reqtype form)
;;            :id id
;;            :css-class css-class
;;            (plist-mapc (lambda (key val)
;;                          (when val
;;                            (htm
;;                             (:input :type "hidden"
;;                                     :name (string-downcase key)
;;                                     :value (lisp->html val)))))
;;                        hidden)
;;            (display body))))



;; (defbrick* brick-form ()
;;   ((reqtype)
;;    hidden
;;    (body :required t)
;;    (action :required t))
;;   (:default-initargs :reqtype "GET")
;;   (with-html
;;     (:form :action action
;;            :method reqtype
;;            :id id
;;            :css-class css-class
;;            (plist-mapc (lambda (key val)
;;                          (when val
;;                            (htm
;;                             (:input :type "hidden"
;;                                     :name (string-downcase key)
;;                                     :value (lisp->html val)))))
;;                        hidden)
;;            (display body))))



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
