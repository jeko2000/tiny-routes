;;;; builder.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.builder
  (:use :cl)
  (:export #:wrap-request-predicate
           #:wrap-request-mapper
           #:wrap-response-mapper
           #:wrap-response-mapper*))

(in-package :tiny-routes.middleware.builder)

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
