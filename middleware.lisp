;;;; request.lisp
(in-package #:tiny-routes)

;; request middleware
(defun wrap-request-matches-method (handler method)
  "Return HANDLER if METHOD is nil or `:any'. Otherwise, wrap
HANDLER such that it is called only if the request method is `eq' to
METHOD."
  (if (or (null method) (eq method :any))
      handler
      (lambda (request)
        (when (eq method (request-method request))
          (funcall handler request)))))

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
(defun response-mapper (handler request response-mapper)
  (let ((response (funcall handler request)))
    (typecase response
      (cons
       (funcall response-mapper response))
      (function
       (lambda (responder)
        (funcall response (lambda (res)
                            (funcall responder (funcall response-mapper res))))))
      (t response))))

(defun wrap-response-mapper (handler response-mapper)
  (lambda (request)
    (response-mapper handler request response-mapper)))

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
