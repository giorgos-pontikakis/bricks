(in-package :bricks)

(define-condition slot-uninitialized (error)
  ((%class :reader %class :initarg :class)
   (%slot  :reader %slot  :initarg :slot))
  (:report (lambda (condition stream)
             (format stream "For objects of class ~A, slot ~A must be initialized."
                     (%class condition) (%slot condition)))))



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

(defgeneric lisp->html (value &key))

(defmethod lisp->html ((value (eql t)) &key)
  +html-true+)

(defmethod lisp->html ((value (eql nil)) &key)
  +html-false+)

(defmethod lisp->html ((value (eql :null)) &key)
  +html-null+)

(defmethod lisp->html ((value integer) &key)
  (format nil "~D" value))

(defmethod lisp->html ((value rational) &key)
  (format nil "~,F" value))

(defmethod lisp->html ((value float) &key)
  (format nil "~,F" value))

(defmethod lisp->html ((value string) &key)
  (escape-string-minimal-plus-quotes value))

(defmethod lisp->html ((value symbol) &key)
  (escape-string-minimal-plus-quotes (string-downcase value)))



;;; ----------------------------------------------------------------------
;;; HTML basic macros
;;; ----------------------------------------------------------------------

(defparameter *indent* nil
  "Controls indentation of generated html/xml from with-document and with-html macros")

(defun indent-mode ()
  "Returns the current indentation mode for with-html and with-document macros"
  *indent*)

(defun (setf indent-mode) (mode)
  "Sets the current indentation mode for with-html and with-document macros"
  (setf *indent* mode))

(defparameter *doctype* :html4
  "Controls doctype and html-mode")

(defun doctype ()
  "Returns the current indentation mode for with-html and with-document macros"
  *doctype*)

(defun (setf doctype) (doctype)
  "Sets the current indentation mode for with-html and with-document macros"
  (setf *doctype* doctype))

(defmacro with-html (&body body)
  ;; We return (values) so that we can use this inside another form
  ;; function without writing the return value of with-html-output,
  ;; which is garbage, to the output string
  (let ((indent (indent-mode)))
    `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
       ,@body
       (values))))

(defmacro defhtml (name args &body body)
  `(defun ,name (&key ,@args)
     (lambda (&key ,@(mapcar #'list args args))
       (with-html
         ,@body))))

(defmacro html ((&rest args) &body body)
  `(lambda (,@args)
     (with-html
       ,@body)))

(defun obj (class &rest args)
  (display (apply #'make-instance class args)))

(defun call (fn &rest args)
  (display (apply fn args)))

(defmacro with-document ((&optional spec &rest html-params) &body body &environment env)
  "When spec is a two-items list, the doctype is the first item of the list and the indentation
mode is the second item. If spec is an atom, indent-mode controls indentation. If spec is null,
doctype controls the doctype and indent-mode controls indentation."
  ;; html-mode is a compile-time flag. (see cl-who source).  Therefore, we use macroexpand to
  ;; force macroexpansion of with-html-output at compile-time _before_ we set html-mode to its
  ;; original value. Otherwise, the compiler may first return from unwind-protect, reseting
  ;; html-mode to its original value and then continue to expand with-html-output.
  ;;
  ;; CLHS 3.2.3.1: However, the order of processing (including macro expansion) of subforms
  ;; that are not top level forms and the order of further compilation is unspecified as long
  ;; as Common Lisp semantics are preserved.
  (let ((old-html-mode (html-mode)))
    (unwind-protect
         (destructuring-bind (&optional doctype indent)
             (cond ((null spec) (list (doctype) (indent-mode)))
                   ((atom spec) (list spec (indent-mode)))
                   (t spec))
           (ecase doctype
             ((:xhtml)
              (setf (html-mode) :xml)
              (macroexpand `(with-html-output-to-string (*standard-output* nil :prologue t
                                                                               :indent ,indent)
                              (:html ,@html-params
                                ,@body))
                           env))
             ((:html4)
              (setf (html-mode) :sgml)
              (macroexpand `(with-html-output-to-string (*standard-output* nil :prologue t
                                                                               :indent ,indent)
                              (:html ,@html-params
                                ,@body))
                           env))
             ((:html5)
              (setf (html-mode) :html5)
              (macroexpand `(with-html-output-to-string (*standard-output* nil :prologue t
                                                                               :indent ,indent)
                              (:html ,@html-params
                                ,@body))
                           env))
             ((:xml)
              (setf (html-mode) :xml)
              (let ((*html-empty-tag-aware-p* nil))
                (macroexpand `(with-html-output-to-string (*standard-output* nil :prologue nil
                                                                                 :indent ,indent)
                                (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
                                (:html ,@html-params
                                  ,@body))
                             env)))
             ((:xhr)
              (setf (html-mode) :xml)
              (let ((*html-empty-tag-aware-p* nil))
                (macroexpand `(with-html-output-to-string (*standard-output* nil :prologue nil
                                                                                 :indent ,indent)
                                (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
                                (:html ,@html-params
                                  ,@body))
                             env)))))
      (setf (html-mode) old-html-mode))))
