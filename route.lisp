;;;; path.lisp
(in-package #:tiny-routes)

(defmacro route (method path-template req-binding &body body)
  (let ((handler-form
          (if (null req-binding)
              (let ((req (gensym)))
                `(lambda (,req)
                   (declare (ignore ,req))
                   ,@body))
              `(lambda ,req-binding
                 ,@body))))
    `(pipe ,handler-form
       (wrap-request-matches-path-template ,path-template)
       (wrap-request-matches-method ,method))))

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

(defmacro define-routes (name &body handlers)
  "Define a route handler from ROUTES"
  `(defparameter ,name (routes ,@handlers)))
