(in-package :bricks)

(declaim (optimize (speed 0) (debug 3)))

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


(defclass widget ()
  ())

(defgeneric display (widget &key)
  (:documentation "Display a widget as html."))

;; (defmethod display ((widget nil) &key)
;;   (declare (ignore widget))
;;   (with-html
;;     ""))

;; (defmethod display ((widget function) &key)
;;   (apply widget))

(defmethod display ((widget list) &key)
  (mapc #'display widget))

(defmethod display ((widget atom))
  (with-html
    (str (lisp->html widget))))