;;;; request.lisp
(in-package #:tiny-routes)

;; selectors
(defmacro with-request-slots (lambda-list request &body body)
  "Bind the variables in LAMBDA-LIST to the corresponding values in
REQUEST and evaluate BODY."
  `(destructuring-bind (&key ,@lambda-list) ,request
     ,@body))

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

(defun request-uri (request &optional default)
  "Return the request URI from REQUEST or DEFAULT."
  (request-get request :request-uri default))

(defun url-scheme (request &optional default)
  "Return the URL scheme from REQUEST or DEFAULT."
  (request-get request :url-scheme default))

(defun remote-addr (request &optional default)
  "Return the remote address from REQUEST or DEFAULT."
  (request-get request :remote-addr default))

(defmacro remote-address (request)
  "Alias for `remote-addr'."
  `(remote-addr ,request))

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

(defun headers (request &optional default)
  "Return the headers from REQUEST or DEFAULT."
  (request-get request :headers default))

(defun path-params (request &optional default)
  "Return the path params from REQUEST or DEFAULT."
  (request-get request :path-params default))

(defun request-body (request &optional default)
  "Return the request body from REQUEST or DEFAULT."
  (request-get request :request-body default))

(defun path-param (request key &optional default)
  "Return the path param value from REQUEST associated with KEY or DEFAULT."
  (let* ((params (path-params request))
         (pair (assoc key params :test #'string=)))
    (if pair
        (cdr pair)
        default)))

(defun request-append (request key value)
  "Return a clone of REQUEST containing KEY and VALUE."
  (plist-append request key value))
