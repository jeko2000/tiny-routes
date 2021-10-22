;;;; request.lisp
(in-package #:tiny-routes)

;; selectors
(defmacro with-request-slots (lambda-list request &body body)
  "Bind the variables in LAMBDA-LIST to the corresponding values in
REQUEST and evaluate BODY."
  `(destructuring-bind (&key ,@lambda-list) ,request
     ,@body))

(defun request-method (request &optional default)
  "Return the request method from REQUEST or DEFAULT."
  (getf request :request-method default))

(defun script-name (request &optional default)
  "Return the script name from REQUEST or DEFAULT."
  (getf request :script-name default))

(defun path-info (request &optional default)
  "Return the path info from REQUEST or DEFAULT."
  (getf request :path-info default))

(defun server-name (request &optional default)
  "Return the server name from REQUEST or DEFAULT."
  (getf request :server-name default))

(defun server-port (request &optional default)
  "Return the server port from REQUEST or DEFAULT."
  (getf request :server-port default))

(defun server-protocol (request &optional default)
  "Return the server protocol from REQUEST or DEFAULT."
  (getf request :server-protocol default))

(defun request-uri (request &optional default)
  "Return the request URI from REQUEST or DEFAULT."
  (getf request :request-uri default))

(defun url-scheme (request &optional default)
  "Return the URL scheme from REQUEST or DEFAULT."
  (getf request :url-scheme default))

(defun remote-addr (request &optional default)
  "Return the remote address from REQUEST or DEFAULT."
  (getf request :remote-addr default))

(defmacro remote-address (request)
  "Alias for `remote-addr'."
  `(remote-addr ,request))

(defun remote-port (request &optional default)
  "Return the remote port from REQUEST or DEFAULT."
  (getf request :remote-port default))

(defun query-string (request &optional default)
  "Return the query string from REQUEST or DEFAULT."
  (getf request :query-string default))

(defun raw-body (request &optional default)
  "Return the raw body from REQUEST or DEFAULT."
  (getf request :raw-body default))

(defun content-length (request &optional default)
  "Return the content length from REQUEST or DEFAULT."
  (getf request :content-length default))

(defun content-type (request &optional default)
  "Return the content type from REQUEST or DEFAULT."
  (getf request :content-type default))

(defun clack.streaming (request &optional default)
  "Return the the clack.streaming from REQUEST or DEFAULT."
  (getf request :clack.streaming default))

(defun clack.io (request &optional default)
  "Return the clack.io from REQUEST or DEFAULT."
  (getf request :clack.io default))

(defun headers (request &optional default)
  "Return the headers from REQUEST or DEFAULT."
  (getf request :headers default))

(defun path-params (request &optional default)
  "Return the path params from REQUEST or DEFAULT."
  (getf request :path-params default))
