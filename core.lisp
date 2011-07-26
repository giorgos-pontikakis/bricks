(in-package :bricks)

(declaim (optimize (speed 0) (debug 3)))



;;; ----------------------------------------------------------------------
;;; Default CL-WHO configuration
;;; ----------------------------------------------------------------------

(setf *attribute-quote-char* #\")
(setf (html-mode) :xml)



;;; ----------------------------------------------------------------------
;;; Lisp -> HTML conversions
;;; ----------------------------------------------------------------------

(defparameter +html-true+ "true")
(defparameter +html-false+ "false")
(defparameter +html-null+ "")

(defgeneric lisp->html (value))

(defmethod lisp->html ((value (eql t)))
  +html-true+)

(defmethod lisp->html ((value (eql :null)))
  +html-null+)

(defmethod lisp->html ((value (eql nil)))
  +html-false+)

(defmethod lisp->html ((value integer))
  (format nil "~D" value))

(defmethod lisp->html ((value rational))
  (format nil "~,2F" value))

(defmethod lisp->html ((value float))
  (format nil "~,4F" value))

(defmethod lisp->html ((value string))
  (escape-string-minimal-plus-quotes value))

(defmethod lisp->html ((value symbol))
  (escape-string-minimal-plus-quotes (string-downcase value)))



;;; ----------------------------------------------------------------------
;;; HTML basic macros
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    ;; We return nil so that we can use this inside another form
    ;; function without writing the return value of with-html-output,
    ;; which is garbage, to the output string
    `(with-html-output (*standard-output* nil :prologue nil :indent nil)
       ,@body
       nil)))

(defmacro defhtml (name args &body body)
  `(defun ,name (&key ,@args)
     (lambda (&key ,@(mapcar #'list args args))
       (with-html
         ,@body))))

(defmacro html ((&rest args) &body body)
  `(lambda (,@args)
     (with-html
       ,@body)))

(defmacro with-document ((&optional spec &rest html-params) &body body)
  (ecase spec
    ((:xhtml nil)
     `(progn
        (setf (html-mode) :xml)
        (with-html-output (*standard-output* nil :prologue t :indent t)
          (:html ,@html-params
            ,@body))))
    ((:html4)
     `(progn
        (setf (html-mode) :sgml)
        (with-html-output (*standard-output* nil :prologue t :indent t)
          (:html ,@html-params
            ,@body))))
    ((:xml)
     `(progn
        (setf (html-mode) :xml)
        (with-html-output (*standard-output* nil :prologue nil :indent t)
          (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
          (:html ,@html-params
            ,@body))))))



;;; ----------------------------------------------------------------------
;;; widgets class
;;; ----------------------------------------------------------------------

(defclass widget ()
  ((id        :accessor id        :initarg :id)
   (css-class :accessor css-class :initarg :css-class)
   (css-style :accessor css-style :initarg :css-style))
  (:default-initargs :id nil :css-class nil :css-style nil))

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
;;; display generic function
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
