;;;; path.lisp
(in-package #:tiny-routes)

(defmacro prepare-route (method path-template req-binding &body body)
  (let ((handler-form
          (if (null req-binding)
              (let ((req (gensym)))
                `(lambda (,req)
                   (declare (ignore ,req))
                   ,@body))
              `(lambda ,req-binding
                 ,@body))))
    `(pipe ,handler-form
       (wrap-middleware)
       ,(when path-template
          `(wrap-request-matches-path-template ,path-template))
       ,(when (and method (not (eq method :any)))
          `(wrap-request-matches-method ,method)))))

(defmacro define-route (req-binding &body body)
  `(prepare-route nil nil ,req-binding ,@body))

(defmacro define-get (path-template req-binding &body body)
  `(prepare-route :get ,path-template ,req-binding ,@body))

(defmacro define-post (path-template req-binding &body body)
  `(prepare-route :post ,path-template ,req-binding ,@body))

(defmacro define-put (path-template req-binding &body body)
  `(prepare-route :put ,path-template ,req-binding ,@body))

(defmacro define-delete (path-template req-binding &body body)
  `(prepare-route :delete ,path-template ,req-binding ,@body))

(defmacro define-head (path-template req-binding &body body)
  `(prepare-route :head ,path-template ,req-binding ,@body))

(defmacro define-options (path-template req-binding &body body)
  `(prepare-route :options ,path-template ,req-binding ,@body))

(defmacro define-any (path-template req-binding &body body)
  `(prepare-route :any ,path-template ,req-binding ,@body))

(defun routes (&rest handlers)
  "Return a handler which itself calls each handler in HANDLERS and
returns the first non-nil response. Otherwise, return nil."
  (lambda (request)
    (loop for handler in handlers
          when (funcall handler request) return it)))

(defmacro define-routes (name &body handlers)
  "Define a route handler from ROUTES"
  `(defparameter ,name (routes ,@handlers)))
