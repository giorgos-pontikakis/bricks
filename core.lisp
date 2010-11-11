(in-package :bricks)

(declaim (optimize (speed 0) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    ;; We return nil so that we can use this inside the render
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

(defun render (html &rest args)
  (cond ((null html) "")
        ((functionp html)
         (apply html args))
        ((listp html)
         (mapc #'render html))
        (t (with-html
             (str (lisp->html html))))))
