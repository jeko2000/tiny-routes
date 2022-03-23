;;;; query-parameters.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.query-parameters
  (:use :cl)
  (:import-from :uiop
                #:split-string)
  (:import-from :tiny-routes.request
                #:query-string
                #:request-append)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-request-mapper)
  (:export #:parse-query-parameters
           #:query-parameter-parser
           #:wrap-query-parameters))

(in-package :tiny-routes.middleware.query-parameters)

(defun parse-query-parameters (query-string)
  (let (params)
    (dolist (pair (split-string query-string :separator '(#\&)))
      (destructuring-bind (&optional key value &rest rest) (uiop:split-string pair :separator '(#\=))
        (when (and key value (null rest))
          (push value params)
          (push (intern key :keyword) params))))
    params))

(defun wrap-query-parameters (handler &key (query-parameter-parser #'parse-query-parameters))
  "Wrap HANDLER such that the results of applying QUERY-PARAMETER-PARSER
to the query string is made available to the request via
`:query-parameters'."
  (wrap-request-mapper
   handler
   (lambda (request)
     (let ((params (funcall query-parameter-parser (query-string request))))
       (request-append request :query-parameters params)))))
