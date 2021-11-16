;;;; request.lisp
(in-package #:tiny-routes)

;; Middleware helpers
(defun wrap-request-predicate (handler request-predicate)
  "Wrap HANDLER such that it is called if request satisfies
REQUEST-PREDICATE."
  (lambda (request)
    (when (funcall request-predicate request)
      (funcall handler request))))

(defun wrap-request-mapper (handler request-mapper)
  "Wrap HANDLER such that it is called with the result of applying
REQUEST-HANDLER to the incoming request if not-nil. Otherwise, return
nil."
  (lambda (request)
    (let ((clone (funcall request-mapper request)))
      (when clone
        (funcall handler clone)))))

(defun wrap-response-mapper* (handler response-mapper)
  "Like `wrap-response-mapper' but pass RESPONSE-MAPPER should be a
function accepting both request and response."
  (lambda (request)
    (let ((response (funcall handler request)))
      (typecase response
        (null nil)
        (cons (funcall response-mapper request response))
        (function
         (lambda (responder)
          (funcall response (lambda (res)
                              (funcall responder
                                       (funcall response-mapper request res))))))))))

(defun wrap-response-mapper (handler response-mapper)
  "Wrap HANDLER such that it returns the result of applying
RESPONSE-MAPPER to response."
  (lambda (request)
    (let ((response (funcall handler request)))
      (typecase response
        (null nil)
        (cons (funcall response-mapper response))
        (function
         (lambda (responder)
          (funcall response (lambda (res)
                              (funcall responder
                                       (funcall response-mapper res))))))))))

(defun wrap-middleware (handler)
  "Wrap HANDLER such that any post-match middleware is wrapped"
  (lambda (request)
    (let ((middleware (request-get request :post-match-middleware #'identity)))
      (funcall (funcall middleware handler) request))))

(defun wrap-post-match-middleware (handler middleware)
  "Wrap HANDLER such that MIDDLEWARE is wrapped after the request is
matched."
  (lambda (request)
    (let ((mw (request-get request :post-match-middleware #'identity)))
      (funcall handler (request-append request :post-match-middleware
                                       (compose mw middleware))))))

;; request middleware
(defun wrap-request-matches-method (handler method)
  "Return HANDLER if METHOD is nil or `:any'. Otherwise, wrap
HANDLER such that it is called only if the request method is `eq' to
METHOD."
  (if (or (null method) (eq method :any))
      handler
      (wrap-request-predicate handler (lambda (req) (eq (request-method req) method)))))

(defun wrap-request-matches-path-template (handler path-template)
  "Return HANDLER if PATH-TEMPLATE is nil or the string \"*\".
Otherwise, wrap HANDLER such that it is called only if the request
path info matches PATH-TEMPLATE. If PATH-TEMPLATE is dynamic, add the
matched params to the request via `:path-params'."
  (if (or (null path-template) (string= "*" path-template))
      handler
      (let ((matcher (make-path-template-matcher path-template)))
        (lambda (request)
          (let ((matches (funcall matcher (request-path-info request))))
            (cond ((null matches)
                   nil)
                  ((consp matches)
                   (funcall handler (request-append request :path-params matches)))
                  (t
                   (funcall handler request))))))))

(defun wrap-request-body (handler)
  "Wrap HANDLER such that the request body is read to a string and made
available to the request via `:request-body'."
  (lambda (request)
    (let ((in (request-raw-body request)))
      (when in
        (let ((request-body (read-stream-to-string in)))
          (funcall handler
                   (request-append request :request-body request-body)))))))

;; response middleware
(defun wrap-response-status (handler status)
  "Wrap HANDLER such that the response's status is set to STATUS."
  (wrap-response-mapper handler (lambda (res) (status-response res status))))

(defun wrap-response-header (handler key value)
  "Wrap HANDLER such that the response's headers includes header with KEY and VALUE."
  (wrap-response-mapper handler (lambda (res) (header-response res key value))))

(defun wrap-response-headers (handler headers)
  "Wrap HANDLER such that the response's headers are set to HEADERS."
  (wrap-response-mapper handler (lambda (res) (headers-response res headers))))

(defun wrap-response-headers-append (handler headers)
  "Wrap HANDLER such that the response's headers include all of HEADERS."
  (wrap-response-mapper handler (lambda (res) (headers-response-append res headers))))

(defun wrap-response-content-type (handler content-type)
  (wrap-response-mapper handler (lambda (res) (content-type-response res content-type))))

(defun wrap-response-body (handler body)
  (wrap-response-mapper handler (lambda (res) (body-response res body))))
