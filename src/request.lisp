;;;; request.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.request
  (:use :cl)
  (:export #:requestp
           #:request
           #:make-request
           #:with-request
           #:request-get
           #:request-method
           #:script-name
           #:path-info
           #:server-name
           #:server-port
           #:server-protocol
           #:uri
           #:url-scheme
           #:remote-address
           #:remote-port
           #:query-string
           #:raw-body
           #:content-length
           #:content-type
           #:clack.streaming
           #:clack.io
           #:request-headers
           #:request-header
           #:request-body
           #:request-append))

(in-package :tiny-routes.request)

;;; Refinements
(defun requestp (object)
  "Return t if OBJECT is a request."
  ;; Naive test that checks if required fields are present in plist
  (when (listp object)
    (destructuring-bind (&key request-uri request-method path-info url-scheme &allow-other-keys)
        object
      (and (stringp request-uri)
           (keywordp request-method)
           (stringp path-info)
           (member url-scheme '("http" "https") :test #'string=)
           t))))

(deftype request ()
  '(satisfies requestp))

;;; Constructors
(defun make-request (&rest args
                     &key
                       (request-uri "/")
                       (request-method :get)
                       (path-info request-uri)
                       (url-scheme "http")
                     &allow-other-keys)
  "Return a new request based on ARGS."
  (flet ((merge-plists (plist1 plist2)
           (loop with plist = (copy-list plist1)
                 for (indicator value) on plist2 by #'cddr
                 do (setf (getf plist indicator) value)
                 finally (return plist))))
    (merge-plists (list :request-uri request-uri
                        :request-method request-method
                        :path-info path-info
                        :url-scheme url-scheme)
                  args)))

;;; Selectors
(defmacro with-request (request-vars request &body body)
  "Bind the variables in REQUEST-VARS to the corresponding values in
REQUEST and evaluate BODY. Each var in REQUEST-VARS can either be a
symbol or a list in the form (variable-name keyword-name
[default-value])."
  (let ((grequest (gensym "request")))
    `(let ((,grequest ,request))
       (let ,(mapcar (lambda (var)
                       (multiple-value-bind (symbol name default)
                           (etypecase var
                             (symbol (values var var nil))
                             (list (values (first var) (second var) (third var))))
                         `(,symbol (getf ,grequest ,(intern (symbol-name name) :keyword) ,default))))
              request-vars)
         ,@body))))

(defun request-get (request key &optional default)
  "Return the value associated with KEY in REQUEST or DEFAULT."
  (getf request key default))

(defun request-method (request &optional default)
  "Return the request method from REQUEST or DEFAULT."
  (request-get request :request-method default))

(defun script-name (request &optional default)
  "Return the script name from REQUEST or DEFAULT."
  (request-get request :script-name default))

(defun path-info (request &optional default)
  "Return the path info from REQUEST or DEFAULT."
  (request-get request :path-info default))

(defun server-name (request &optional default)
  "Return the server name from REQUEST or DEFAULT."
  (request-get request :server-name default))

(defun server-port (request &optional default)
  "Return the server port from REQUEST or DEFAULT."
  (request-get request :server-port default))

(defun server-protocol (request &optional default)
  "Return the server protocol from REQUEST or DEFAULT."
  (request-get request :server-protocol default))

(defun uri (request &optional default)
  "Return the request URI from REQUEST or DEFAULT."
  (request-get request :request-uri default))

(defun url-scheme (request &optional default)
  "Return the URL scheme from REQUEST or DEFAULT."
  (request-get request :url-scheme default))

(defun remote-address (request &optional default)
  "Return the remote address from REQUEST or DEFAULT."
  (request-get request :remote-addr default))

(defun remote-port (request &optional default)
  "Return the remote port from REQUEST or DEFAULT."
  (request-get request :remote-port default))

(defun query-string (request &optional default)
  "Return the query string from REQUEST or DEFAULT."
  (request-get request :query-string default))

(defun raw-body (request &optional default)
  "Return the raw body from REQUEST or DEFAULT."
  (request-get request :raw-body default))

(defun content-length (request &optional default)
  "Return the content length from REQUEST or DEFAULT."
  (request-get request :content-length default))

(defun content-type (request &optional default)
  "Return the content type from REQUEST or DEFAULT."
  (request-get request :content-type default))

(defun clack.streaming (request &optional default)
  "Return the the clack.streaming from REQUEST or DEFAULT."
  (request-get request :clack.streaming default))

(defun clack.io (request &optional default)
  "Return the clack.io from REQUEST or DEFAULT."
  (request-get request :clack.io default))

(defun request-headers (request &optional default)
  "Return the headers from REQUEST or DEFAULT."
  (request-get request :headers default))

(defun request-header (request key &optional default)
  "Return the header value from REQUEST associated with KEY or DEFAULT."
  (let ((headers (request-headers request)))
    (gethash key headers default)))

(defun request-body (request &optional default)
  "Return the request body from REQUEST or DEFAULT."
  (request-get request :request-body default))

;;; Combinations
(defun request-append (request key value)
  "Return a clone of REQUEST containing KEY and VALUE."
  (append (list key value) request))
