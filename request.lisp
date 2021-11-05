;;;; request.lisp
(in-package #:tiny-routes)

;; selectors
(defmacro with-request-slots (lambda-list request &body body)
  "Bind the variables in LAMBDA-LIST to the corresponding values in
REQUEST and evaluate BODY."
  `(destructuring-bind (&key ,@lambda-list &allow-other-keys) ,request
     ,@body))

(defun request-get (request key &optional default)
  "Return the value associated with KEY in REQUEST or DEFAULT."
  (getf request key default))

(defun request-method (request &optional default)
  "Return the request method from REQUEST or DEFAULT."
  (request-get request :request-method default))

(defun request-script-name (request &optional default)
  "Return the script name from REQUEST or DEFAULT."
  (request-get request :script-name default))

(defun request-path-info (request &optional default)
  "Return the path info from REQUEST or DEFAULT."
  (request-get request :path-info default))

(defun request-server-name (request &optional default)
  "Return the server name from REQUEST or DEFAULT."
  (request-get request :server-name default))

(defun request-server-port (request &optional default)
  "Return the server port from REQUEST or DEFAULT."
  (request-get request :server-port default))

(defun request-server-protocol (request &optional default)
  "Return the server protocol from REQUEST or DEFAULT."
  (request-get request :server-protocol default))

(defun request-uri (request &optional default)
  "Return the request URI from REQUEST or DEFAULT."
  (request-get request :request-uri default))

(defun request-url-scheme (request &optional default)
  "Return the URL scheme from REQUEST or DEFAULT."
  (request-get request :url-scheme default))

(defun request-remote-address (request &optional default)
  "Return the remote address from REQUEST or DEFAULT."
  (request-get request :remote-addr default))

(defun request-remote-port (request &optional default)
  "Return the remote port from REQUEST or DEFAULT."
  (request-get request :remote-port default))

(defun request-query-string (request &optional default)
  "Return the query string from REQUEST or DEFAULT."
  (request-get request :query-string default))

(defun request-raw-body (request &optional default)
  "Return the raw body from REQUEST or DEFAULT."
  (request-get request :raw-body default))

(defun request-content-length (request &optional default)
  "Return the content length from REQUEST or DEFAULT."
  (request-get request :content-length default))

(defun request-content-type (request &optional default)
  "Return the content type from REQUEST or DEFAULT."
  (request-get request :content-type default))

(defun request-clack.streaming (request &optional default)
  "Return the the clack.streaming from REQUEST or DEFAULT."
  (request-get request :clack.streaming default))

(defun request-clack.io (request &optional default)
  "Return the clack.io from REQUEST or DEFAULT."
  (request-get request :clack.io default))

(defun request-headers (request &optional default)
  "Return the headers from REQUEST or DEFAULT."
  (request-get request :headers default))

(defun request-header (request key &optional default)
  "Return the header value from REQUEST associated with KEY or DEFAULT."
  (let ((headers (request-headers request)))
    (gethash key headers default)))

(defun request-path-params (request &optional default)
  "Return the path params from REQUEST or DEFAULT."
  (request-get request :path-params default))

(defun request-body (request &optional default)
  "Return the request body from REQUEST or DEFAULT."
  (request-get request :request-body default))

(defun request-path-param (request key &optional default)
  "Return the path param value from REQUEST associated with KEY or DEFAULT."
  (let* ((params (request-path-params request))
         (pair (assoc key params :test #'string=)))
    (if pair
        (cdr pair)
        default)))

(defun request-append (request key value)
  "Return a clone of REQUEST containing KEY and VALUE."
  (plist-append request key value))
