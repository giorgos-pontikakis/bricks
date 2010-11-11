(in-package :bricks)

(defun see-other (url)
  (redirect url :code +http-see-other+))


(defun url (&rest args)
  "Take a list of arguments, convert them to html, concatenate and
print them as a url. If the first argument does not seem to be an
absolute url reference, prepend the webroot, otherwise print the
scheme and machine normally. "
  (with-output-to-string (*standard-output*)
    (unless (null args)
      (bind ((#(scheme machine nil script-name)
               (nth-value 1 (scan-to-strings "(https?:|ftp:)?(//[^/]+)?(/?)(.*)" (first args)))))
        (if machine
            (progn (princ (or scheme "http:"))
                   (princ machine)
                   (princ "/")
                   (princ script-name)
                   (mapc #'princ
                         (mapcar #'lisp->html (rest args))))
            (progn (princ (webroot (package-webapp)))
                   (mapc #'princ
                         (mapcar #'lisp->html args))))))))