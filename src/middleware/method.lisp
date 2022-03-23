;;;; method.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.method
  (:use :cl)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-request-predicate)
  (:import-from :tiny-routes.request
                #:request-method)
  (:export #:wrap-request-matches-method))

(in-package :tiny-routes.middleware.method)

(defun wrap-request-matches-method (handler method)
  "Wrap HANDLER such that it is called only if the request method matches METHOD.

If METHOD is t, nil or `:any', then return HANDLER unchanged.

If METHOD is a list, then wrap handler such that it is called only if
the request method is in the list."
  (check-type method (or symbol list))
  (cond ((or (null method) (eq method t) (eq method :any)) handler)
        ((symbolp method) (wrap-request-predicate
                           handler (lambda (req) (eq (request-method req) method))))
        ((listp method) (wrap-request-predicate
                         handler (lambda (req) (member (request-method req) method))))))
