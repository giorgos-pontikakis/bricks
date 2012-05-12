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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    ;; We return nil so that we can use this inside another form
    ;; function without writing the return value of with-html-output,
    ;; which is garbage, to the output string
    `(with-html-output (*standard-output* nil :prologue nil :indent *indent*)
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

;; (defmacro with-document ((&optional spec &rest html-params) &body body)
;;   "When spec is a two-items list, the doctype is the first item of the list and the indentation
;; mode is the second item. If spec is an atom, indent-mode controls indentation. If spec is null,
;; doctype controls the doctype and indent-mode controls indentation."
;;   (let ((html-mode (gensym)))
;;     (destructuring-bind (&optional doctype indent) (cond ((null spec) (list (doctype) (indent-mode)))
;;                                                          ((atom spec) (list spec (indent-mode)))
;;                                                          (t spec))
;;       (ecase doctype
;;         ((:xhtml)
;;          `(let ((,html-mode (html-mode)))
;;             (unwind-protect (progn
;;                               (setf (html-mode) :xml)
;;                               (with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;                                 (:html ,@html-params
;;                                   ,@body)))
;;               (setf (html-mode) ,html-mode))))
;;         ((:html4)
;;          `(let ((,html-mode (html-mode)))
;;             (unwind-protect (progn
;;                               (setf (html-mode) :sgml)
;;                               (with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;                                 (:html ,@html-params
;;                                   ,@body)))
;;               (setf (html-mode) ,html-mode))))
;;         ((:xml)
;;          `(let ((,html-mode (html-mode)))
;;             (unwind-protect (progn
;;                               (setf (html-mode) :xml)
;;                               (with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;                                 (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
;;                                 (:html ,@html-params
;;                                   ,@body)))
;;               (setf (html-mode) ,html-mode))))
;;         ((:html5)
;;          `(let ((,html-mode (html-mode)))
;;             (unwind-protect (progn
;;                               (setf (html-mode) :sgml)
;;                               (with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;                                 (fmt "<!DOCTYPE html>")
;;                                 (:html ,@html-params
;;                                   ,@body)))
;;               (setf (html-mode) ,html-mode))))))))

;; (defmacro with-document ((&optional spec &rest html-params) &body body)
;;   "When spec is a two-items list, the doctype is the first item of the list and the indentation
;; mode is the second item. If spec is an atom, indent-mode controls indentation. If spec is null,
;; doctype controls the doctype and indent-mode controls indentation."
;;   (with-gensyms (html-mode)
;;     (destructuring-bind (&optional doctype indent) (cond ((null spec) (list (doctype) (indent-mode)))
;;                                                          ((atom spec) (list spec (indent-mode)))
;;                                                          (t spec))
;;       (ecase doctype
;;         ((:xhtml)
;;          (let ((html-mode (html-mode)))
;;            (setf (html-mode) :xml)
;;            (unwind-protect
;;                 `(with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;                    (:html ,@html-params
;;                      ,@body))
;;              (setf (html-mode) html-mode))))
;;         ((:html4)
;;          `(let ((,html-mode (html-mode)))
;;             (setf (html-mode) :sgml)
;;             (unwind-protect
;;                  (with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;                    (:html ,@html-params
;;                      ,@body))
;;               (setf (html-mode) ,html-mode))))
;;         ((:xml)
;;          (let ((html-mode (html-mode)))
;;            (setf (html-mode) :xml)
;;            (unwind-protect
;;                 `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;                    (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
;;                    (:html ,@html-params
;;                      ,@body))
;;              (setf (html-mode) html-mode))))
;;         ((:html5)
;;          (let ((html-mode (html-mode)))
;;            (setf (html-mode) :sgml)
;;            (unwind-protect
;;                 `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;                    (fmt "<!DOCTYPE html>")
;;                    (:html ,@html-params
;;                      ,@body))
;;              (setf (html-mode) html-mode))))))))


;; (defmacro with-document ((&optional spec &rest html-params) &body body)
;;   "When spec is a two-items list, the doctype is the first item of the list and the indentation
;; mode is the second item. If spec is an atom, indent-mode controls indentation. If spec is null,
;; doctype controls the doctype and indent-mode controls indentation."
;;   (destructuring-bind (&optional doctype indent) (cond ((null spec) (list (doctype) (indent-mode)))
;;                                                        ((atom spec) (list spec (indent-mode)))
;;                                                        (t spec))
;;     (ecase doctype
;;       ((:xhtml)
;;        (let ((html-mode (html-mode)))
;;          (setf (html-mode) :xml)
;;          `(with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;             (:html ,@html-params
;;               ,@body))))
;;       ((:html4)
;;        (let ((html-mode (html-mode)))
;;          (setf (html-mode) :sgml)
;;          `(with-html-output (*standard-output* nil :prologue t :indent ,indent)
;;             (:html ,@html-params
;;               ,@body))))
;;       ((:xml)
;;        (let ((html-mode (html-mode)))
;;          (setf (html-mode) :xml)
;;          `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;             (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
;;             (:html ,@html-params
;;               ,@body))))
;;       ((:html5)
;;        (let ((html-mode (html-mode)))
;;          (setf (html-mode) :sgml)
;;          `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
;;             (fmt "<!DOCTYPE html>")
;;             (:html ,@html-params
;;               ,@body)))))))

(defmacro with-document ((&optional spec &rest html-params) &body body)
  "When spec is a two-items list, the doctype is the first item of the list and the indentation
mode is the second item. If spec is an atom, indent-mode controls indentation. If spec is null,
doctype controls the doctype and indent-mode controls indentation."
  (destructuring-bind (&optional doctype indent) (cond ((null spec) (list (doctype) (indent-mode)))
                                                       ((atom spec) (list spec (indent-mode)))
                                                       (t spec))
    (ecase doctype
      ((:xhtml)
       (progn
         (setf (html-mode) :xml)
         `(with-html-output (*standard-output* nil :prologue t :indent ,indent)
            (:html ,@html-params
              ,@body))))
      ((:html4)
       (progn
         (setf (html-mode) :sgml)
         `(with-html-output (*standard-output* nil :prologue t :indent ,indent)
            (:html ,@html-params
              ,@body))))
      ((:xml)
       (progn
         (setf (html-mode) :xml)
         `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
            (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
            (:html ,@html-params
              ,@body))))
      ((:html5)
       (progn
         (setf (html-mode) :sgml)
         `(with-html-output (*standard-output* nil :prologue nil :indent ,indent)
            (fmt "<!DOCTYPE html>")
            (:html ,@html-params
              ,@body)))))))





;;; ----------------------------------------------------------------------
;;; widgets class
;;; ----------------------------------------------------------------------

(defclass widget ()
  ((id        :accessor id        :initarg :id)
   (css-class :accessor css-class :initarg :css-class))
  (:default-initargs :id nil
                     :css-class nil))

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
