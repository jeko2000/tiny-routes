;;;; path-template.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.path-template
  (:use :cl)
  (:import-from :cl-ppcre
                #:create-scanner
                #:do-matches-as-strings
                #:regex-replace-all
                #:scan
                #:scan-to-strings)
  (:import-from :tiny-routes.request
                #:request-append
                #:request-get
                #:path-info)
  (:import-from :tiny-routes.util
                #:compose)
  (:export #:path-parameter
           #:with-path-parameters
           #:wrap-request-path-info-matcher
           #:wrap-request-matches-path-template))

(in-package :tiny-routes.middleware.path-template)

(defvar *path-token-scanner* (create-scanner ":([A-Za-z_][A-Za-z0-9_-]*)")
  "The cl-ppcre scanner that matches against a path token such as :id in /articles/:id.")

(defun make-path-template-exact-matcher (path-template)
  "Return a closure that accepts a path and returns t when path matches
PATH-TEMPLATE."
  (check-type path-template string)
  (lambda (path)
    (string= path path-template)))

(defun make-path-template-keyword-matcher (path-template)
  "Return a closure that accepts a path and returns an plist of matched
groups when path matches PATH-TEMPLATE.

For example, a path template for \"/users/:user-id\" when matched
against \"/users/1042\" returns `(:user-id \"jeko\")'."
  (check-type path-template string)
  (flet ((parse-token-names (path-template)
           (let (names)
             (ppcre:do-matches-as-strings (match *path-token-scanner* path-template)
               ;; chop off the : character
               (push (subseq match 1) names))
             (nreverse names)))
         (make-template-scanner (path-template)
           (ppcre:create-scanner
            (concatenate 'string
                         "^"
                         (ppcre:regex-replace-all *path-token-scanner* path-template "([^/]+)")
                         "$"))))
    (let ((token-names (parse-token-names path-template))
          (template-scanner (make-template-scanner path-template)))
      (lambda (path)
        (let ((token-values (nth-value 1 (ppcre:scan-to-strings template-scanner path))))
          (when token-values
            (loop
              for name in token-names
              for value across token-values
              collect (intern (string-upcase name) :keyword)
              collect value)))))))

(defun make-path-template-regex-matcher (path-template)
  "Return a closure that accepts a path and returns non-nil if path
matches the regex in PATH-TEMPLATE."
  (check-type path-template string)
  (let ((template-scanner (create-scanner path-template)))
    (lambda (path)
      ;; TODO: Consider returning the matched groups
      (and (scan template-scanner path) t))))

(defun make-path-template-wildcard-matcher (path-template)
  "Docstring"
  (check-type path-template string)
  (flet ((make-template-scanner (path-template)
	   ;; Metacharacters need to be matched literally
	   (let ((path-template-meta (ppcre:regex-replace-all "[.]"
							      path-template
							      "[.]")))
	     (ppcre:create-scanner
	      (concatenate 'string
			   "^"
			   (ppcre:regex-replace-all "[*]" path-template-meta "(.+)")
			   "$")))))
    (let ((template-scanner (make-template-scanner path-template)))
      (lambda (path)
	(let ((path-params
		(nth-value 1 (ppcre:scan-to-strings template-scanner path))))
	  (loop for i from 0 to (1- (length path-params))
		collect (aref path-params i)))))))

(defun wrap-request-path-info-matcher (handler path-info-matcher)
  "Wrap HANDLER such that it is called only if the result of applying
PATH-INFO-MATCHER to the request's path-info returns non-nil.

If the result of applying PATH-INFO-MATCHER to the request is a list,
then it is made available to the request under `:path-parameters'."
  (check-type path-info-matcher function)
  (lambda (request)
    (let* ((path-info (path-info request ""))
           (params (funcall path-info-matcher path-info)))
      (cond ((null params) nil)
            ((listp params) (funcall handler (request-append request :path-parameters params)))
            (t (funcall handler request))))))

(defun wrap-request-matches-path-template (handler path-template &key regex)
  "Wrap HANDLER such that it is called only if the request path matches
the PATH-TEMPLATE.

If PATH-TEMPLATE is t, nil, the empty string, or \"*\", then return
HANDLER unchanged.

If REGEX is non-nil, then interpret path-template as a regular
expression."
  (check-type path-template (or symbol string))
  (cond ((or (null path-template)
             (eq path-template t)
             (string= path-template "")
             (string= path-template "*"))
         handler)
        ;; If regex is non-nil, then interpret path-info as a regex
        (regex
         (wrap-request-path-info-matcher handler (make-path-template-regex-matcher path-template)))
	((find #\* path-template)
	 (wrap-request-path-info-matcher handler (make-path-template-wildcard-matcher path-template)))
        ;; A `:' in the path template means it is a keyword template
        ((find #\: path-template)
         (wrap-request-path-info-matcher handler (make-path-template-keyword-matcher path-template)))
        (t
         (wrap-request-path-info-matcher handler (make-path-template-exact-matcher path-template)))))

(defun path-parameter (request path-parameter &optional default)
  "Return the value mapped to PATH-PARAMETER from REQUEST or DEFAULT."
  (getf (getf request :path-parameters) path-parameter default))

(defmacro with-path-parameters (vars path-parameters &body body)
  "Bind the variables in VARS to the corresponding values present in
PATH-PARAMETERS."
  (let ((gpath-parameters (gensym "path-parameters")))
    `(let* ((,gpath-parameters ,path-parameters))
       (destructuring-bind (&key ,@vars &allow-other-keys) ,gpath-parameters
         ,@body))))
