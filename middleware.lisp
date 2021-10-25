;;;; request.lisp
(in-package #:tiny-routes)

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
          (let ((matches (funcall matcher (path-info request))))
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
    (let ((in (raw-body request)))
      (when in
        (let ((request-body (read-stream-to-string in)))
          (funcall handler
                   (request-append request :request-body request-body)))))))
