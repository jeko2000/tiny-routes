;;;; middleware.lisp

;;; A middleware is a function that accepts a handler as its first
;;; argument, optionally accepts other arguments, and returns a
;;; handler.

(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware
  (:use :cl)
  (:import-from :uiop
                #:if-let)
  (:import-from :tiny-routes.request
                #:request-append
                #:request-method
                #:request-get
                #:raw-body
                #:path-info)
  (:import-from :tiny-routes.response
                #:append-header
                #:status-response
                #:headers-response
                #:header-response
                #:body-response
                #:response-body)
  (:export #:wrap-request-predicate
           #:wrap-request-mapper
           #:wrap-response-mapper
           #:wrap-response-mapper*
           #:pipe
           #:wrap-request-matches-method
           #:make-path-matcher
           #:wrap-request-matches-path-template
           #:wrap-request-body)
  (:export #:wrap-response-status
           #:wrap-response-header
           #:wrap-response-headers
           #:wrap-response-content-type
           #:wrap-response-body
           #:wrap-response-body-mapper))

(in-package :tiny-routes.middleware)

(defvar *path-token-regex* ":([A-Za-z_][A-Za-z0-9_-]*)"
  "The regular expression used to match against a path token such as :id in /articles/:id.")

(defvar *path-token-scanner* (cl-ppcre:create-scanner *path-token-regex*)
  "The cl-ppcre scanner that matches path tokens.")

;;; Middleware util functions
(defun wrap-request-predicate (handler request-predicate)
  "Returns a new handler that calls HANDLER only if the request
satisfies REQUEST-PREDICATE."
  (lambda (request)
    (when (funcall request-predicate request)
      (funcall handler request))))

(defun wrap-request-mapper (handler request-mapper)
  "Return a new handler that calls HANDLER with the result of applying
REQUEST-MAPPER to request."
  (lambda (request)
    (funcall handler (funcall request-mapper request))))

(defun wrap-response-mapper (handler response-mapper)
  "Wrap HANDLER such that it returns the result of applying
RESPONSE-MAPPER to response."
  (lambda (request)
    (let ((response (funcall handler request)))
      (typecase response
        (null nil)
        (cons (funcall response-mapper response))
        ;; Clack allows async response in the form of a lambda
        (function
         (lambda (responder)
          (funcall response (lambda (res)
                              (funcall responder
                                       (funcall response-mapper res))))))))))

(defun wrap-response-mapper* (handler bi-mapper)
  "Wrap HANDLER such that it returns the result of applying BI-MAPPER
to request and response."
  (lambda (request)
    (let ((response (funcall handler request)))
      (typecase response
        (null nil)
        (cons (funcall bi-mapper request response))
        (function
         (lambda (responder)
          (funcall response (lambda (res)
                              (funcall responder
                                       (funcall bi-mapper request res))))))))))

(defmacro pipe (value &body wrappers)
  "Pipe VALUE through WRAPPERS by subsequently passing each wrapped
value as the first parameter of the next wrapper."
  (reduce
   (lambda (w1 w2)
     (cond ((null w1) w2)
           ((null w2) w1)
           (t
            (let ((w2 (cond ((symbolp w2) `(,w2))
                            (t w2))))
              `(,(car w2) ,w1 ,@(cdr w2))))))
   wrappers :initial-value value))

;;; Request middleware
(defun wrap-request-matches-method (handler method)
  "Return HANDLER if METHOD is nil or `:any'. Otherwise, wrap
HANDLER such that it is called only if the request method is `eq' to
METHOD."
  (if (or (null method) (eq method :any))
      handler
      (wrap-request-predicate handler (lambda (req) (eq (request-method req) method)))))

(defun make-path-matcher (path-template)
  "Return a closure that accepts a path and returns an alist of matched
groups if path matches PATH-TEMPLATE.

For example, a path template for \"/users/:user-id\" when matched
against \"/users/1042\" returns `(:user-id \"jeko\")'."
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

(defun wrap-request-matches-path-template (handler path-template)
  "Return HANDLER if PATH-TEMPLATE is nil or the string \"*\".
Otherwise, wrap HANDLER such that it is called only if the request
path info matches PATH-TEMPLATE. If PATH-TEMPLATE is dynamic, add the
matched params to the request via `:path-params'."
  (if (or (null path-template) (string= "*" path-template))
      handler
      (let ((matcher (make-path-matcher path-template)))
        (lambda (request)
          (if-let ((matches (funcall matcher (path-info request))))
            (funcall handler (request-append request :path-params matches))
            (funcall handler request))))))

(defun read-stream-to-string (input-stream)
  "Read INPUT-STREAM entirely and return the contents as a string."
  (when input-stream)
  (with-output-to-string (output-stream)
    (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
      (loop for pos = (read-sequence buf input-stream)
            while (plusp pos)
            do (write-sequence buf output-stream :end pos)))))

(defun wrap-request-body (handler)
  "Wrap HANDLER such that the request body is read to a string and made
available to the request via `:request-body'."
  (lambda (request)
    (let* ((stream (raw-body request))
           (body (read-stream-to-string stream)))
      (funcall handler (request-append request :request-body body)))))

;;; Response middleware
(defun wrap-response-status (handler status)
  "Wrap HANDLER such that the response status is set to STATUS."
  (wrap-response-mapper handler (lambda (res) (status-response res status))))

(defun wrap-response-header (handler key value)
  "Wrap HANDLER such that the response headers includes header with KEY and VALUE."
  (wrap-response-mapper handler (lambda (res) (header-response res key value))))

(defun wrap-response-headers (handler headers)
  "Wrap HANDLER such that the response headers are set to HEADERS."
  (wrap-response-mapper handler (lambda (res) (headers-response res headers))))

(defun wrap-response-content-type (handler content-type)
  "Wrap HANDLER such that the response content type is set to CONTENT-TYPE."
  (wrap-response-header handler :content-type content-type))

(defun wrap-response-body (handler body)
  (wrap-response-mapper handler (lambda (res) (body-response res body))))

(defun wrap-response-body-mapper (handler body-mapper)
  (wrap-response-mapper handler (lambda (res) (funcall body-mapper (response-body res)))))
