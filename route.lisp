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
    `(wrap-request-matches-method
      (wrap-request-matches-path-template
       ,handler-form ,path-template)
      ,method)))

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

(defmacro defroutes (name &rest handlers)
  "Define a route handler from ROUTES"
  `(defparameter ,name (routes ,@handlers)))
