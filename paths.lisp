(in-package :bricks)

;;; auxiliary

(defun princ-url (args)
  (mapc (lambda (arg)
          (princ (if (symbolp arg)
                     (get-web-path arg)
                     (lisp->html arg))))
        args))



;;; exported stuff

(defun see-other (url)
  (redirect url :code +http-see-other+))

(defun url* (&rest args)
  "Concatenates its arguments to produce a url path. Expects string or
symbols as arguments. Symbols are used as keys to get values from
web-paths of package-webapp."
  (unless (null args)
    (with-output-to-string (*standard-output*)
      (princ-url args))))

(defun url (&rest args)
  "Same as url*, but it prepends web-root of package-webapp"
  (unless (null args)
    (with-output-to-string (*standard-output*)
      (progn (princ (web-root (package-webapp)))
             (princ-url args)))))

(defun path (&rest args)
  "Concatenates its arguments, prepending fs-root, to produce an
  absolute pathname. Expects string or symbols as arguments. Symbols
  are used as keys to get values from fs-paths of package-webapp. If
  the final element is a string not ending with a slash, it is treated
  as filename "
  (let ((path (mapcar (lambda (arg)
                        (cond ((symbolp arg)
                               (get-fs-path arg))
                              ((stringp arg)
                               (make-pathname :directory `(:relative ,@(split "/" arg))))
                              ((pathnamep arg)
                               arg)
                              (t (error "Invalid input."))))
                      args)))
    ;; If the final element does not end with a slash, convert to a
    ;; file pathname and replace it in the path list destructively
    (let ((final (car (last args))))
      (when (and (stringp final)
                 (not (alexandria:emptyp final))
                 (not (alexandria:ends-with #\/ final)))
        (rplaca (last path) (pathname-as-file (car (last path))))))
    (reduce (lambda (path1 path2)
              (merge-pathnames path1 path2))
            (nreverse (cons (fs-root (package-webapp)) path)))))


(defun url->path (url)
  (path (subseq url (length (web-root (package-webapp))))))

(defun path->url (pathname &optional root)
  (url (enough-namestring pathname
                          (or root
                              (fs-root (package-webapp))))))

(defun path->url* (pathname &optional root)
  (url* (enough-namestring pathname
                           (or root
                               (fs-root (package-webapp))))))