;;;; path.lisp
(in-package #:tiny-routes)

(defun %wrap-route-matches-method (handler method)
  "Return HANDLER if METHOD is nil or `:any'. Otherwise, wrap
HANDLER such that it is called only if the request method is `eq' to
METHOD."
  (if (or (null method) (eq method :any))
      handler
      (lambda (request)
        (when (eq method (request-method request))
          (funcall handler request)))))

(defun %wrap-route-matches-path-template (handler path-template)
  "Wrap HANDLER such that it is called only if the request path info
matches PATH-TEMPLATE."
  (let ((matcher (make-path-template-matcher path-template)))
    (lambda (request)
      (let ((matches (funcall matcher (path-info request))))
        (cond ((null matches)
               nil)
              ((consp matches)
               (funcall handler (append (list :path-params matches) request)))
              (t
               (funcall handler request)))))))

(defun %wrap-route-matches (handler method path-template)
  (%wrap-route-matches-method
   (%wrap-route-matches-path-template
    handler path-template)
   method))

(defmacro route (method path-template req-binding &body body)
  (let ((handler-form
          (if (null req-binding)
              (let ((req (gensym)))
                `(lambda (,req)
                   (declare (ignore ,req))
                   ,@body))
              `(lambda ,req-binding
                 ,@body))))
    `(%wrap-route-matches ,handler-form ,method ,path-template)))

(defmacro route-get (path-template req-binding &body body)
  `(route :get ,path-template ,req-binding ,@body))

(defmacro route-post (path-template req-binding &body body)
  `(route :post ,path-template ,req-binding ,@body))

(defmacro route-put (path-template req-binding &body body)
  `(route :put ,path-template ,req-binding ,@body))

(defmacro route-delete (path-template req-binding &body body)
  `(route :delete ,path-template ,req-binding ,@body))

(defmacro route-head (path-template req-binding &body body)
  `(route :head ,path-template ,req-binding ,@body))

(defmacro route-options (path-template req-binding &body body)
  `(route :options ,path-template ,req-binding ,@body))

(defmacro route-any (path-template req-binding &body body)
  `(route :any ,path-template ,req-binding ,@body))

(defun routes (&rest handlers)
  "Return a handler which itself calls each handler in HANDLERS and
returns the first non-nil response. Otherwise, return nil."
  (lambda (request)
    (loop for handler in handlers
          when (funcall handler request) return it)))
