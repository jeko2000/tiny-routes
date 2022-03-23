;;;; route.lisp

;;; A route is just a handler wrapped with middleware to allow it to
;;; `match' a given request by HTTP method and or request URI.
;;;
;;; Routes also add support for `post-match' middleware which are
;;; middleware called after the HTTP method and request URI is
;;; matched.

(in-package :cl-user)
(uiop:define-package :tiny-routes
  (:use :cl)
  (:nicknames :tiny)
  (:import-from :uiop #:if-let)
  (:use-reexport
   :tiny-routes.request
   :tiny-routes.response
   :tiny-routes.middleware
   :tiny-routes.util)
  (:export #:wrap-middleware-internal
           #:wrap-post-match-middleware
           #:define-route
           #:define-get
           #:define-post
           #:define-put
           #:define-delete
           #:define-head
           #:define-options
           #:define-any
           #:routes
           #:define-routes))

(in-package :tiny-routes)

(defun wrap-middleware-internal (handler)
  "Wrap HANDLER such that post-match handlers are called."
  (lambda (request)
    (if-let ((post-match-middleware (request-get request :post-match-middleware)))
      (funcall (funcall post-match-middleware handler) request)
      (funcall handler request))))

(defun wrap-post-match-middleware (handler middleware)
  "Wrap HANDLER such that MIDDLEWARE is wrapped after the request is
matched."
  (lambda (request)
    (let ((mw (request-get request :post-match-middleware #'identity)))
      (funcall handler (request-append request :post-match-middleware
                                       (compose mw middleware))))))

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
       (wrap-middleware-internal)
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
