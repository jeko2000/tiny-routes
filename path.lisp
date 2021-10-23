;;;; path.lisp
(in-package #:tiny-routes)

(defvar *path-token-regex* ":([A-Za-z_][A-Za-z0-9_-]*)"
  "The regular expression used to match against a path token such as :id in /articles/:id.")

(defvar *path-token-scanner* (cl-ppcre:create-scanner *path-token-regex*)
  "The cl-ppcre scanner that matches path tokens")

(defun make-path-template-matcher (path-template)
  "Return a closure that accepts a path and returns non-nil if path
matches PATH-TEMPLATE."
  (cond ((or (string= "*" path-template) (string= "/*" path-template))
         (constantly t))
        ((find #\: path-template)
         (make-regex-path-matcher path-template))
        (t
         (lambda (path)
           (string= path path-template)))))

(defun %parse-token-names (path-template)
  "Return a list of token names in PATH-TEMPLATE."
  (let ((names '()))
    (cl-ppcre:do-matches-as-strings (match *path-token-scanner* path-template)
      ;; chop off the : character
      (push (subseq match 1) names))
    (nreverse names)))

(defun %make-path-template-regex (path-template)
  "Return a regex that matches paths against PATH-TEMPLATE."
  (concatenate 'string
               "^"
               (cl-ppcre:regex-replace-all *path-token-scanner* path-template "([^/]+)")
               "$"))

(defun make-regex-path-matcher (path-template)
  "Return a closure that accepts a path and returns an alist of
matched groups if path matches PATH-TEMPLATE."
  (let ((token-names (%parse-token-names path-template))
        (scanner (cl-ppcre:create-scanner (%make-path-template-regex path-template))))
    (lambda (path)
      (let ((token-values (nth-value 1 (cl-ppcre:scan-to-strings scanner path))))
        (when token-values
          (loop
            for name in token-names
            for value across token-values
            collect (cons name value)))))))
