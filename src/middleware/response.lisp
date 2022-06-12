;;;; response.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.response
  (:use :cl)
  (:import-from :tiny-routes.response
                #:response-body
                #:response-headers
                #:clone-response)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-response-mapper)
  (:export #:wrap-response-status
           #:wrap-response-header
           #:wrap-response-headers
           #:wrap-response-headers-append
           #:wrap-response-content-type
           #:wrap-response-body
           #:wrap-response-body-mapper))

(in-package :tiny-routes.middleware.response)

(defun wrap-response-status (handler status)
  "Wrap HANDLER such that the response status is set to STATUS."
  (wrap-response-mapper handler (lambda (res) (clone-response res :status status))))

(defun wrap-response-header (handler key value)
  "Wrap HANDLER such that the response headers includes header with KEY and VALUE."
  (wrap-response-mapper
   handler
   (lambda (res) (clone-response res :headers (append (list key value) (response-headers res))))))

(defun wrap-response-headers (handler headers)
  "Wrap HANDLER such that the response headers are set to HEADERS."
  (wrap-response-mapper handler (lambda (res) (clone-response res :headers headers))))

(defun wrap-response-headers-append (handler headers)
  "Wrap HANDLER such that the response headers include HEADERS."
  (wrap-response-mapper
   handler
   (lambda (res) (clone-response res :headers (append headers (response-headers res))))))

(defun wrap-response-content-type (handler content-type)
  "Wrap HANDLER such that the response content type is set to CONTENT-TYPE."
  (wrap-response-header handler :content-type content-type))

(defun wrap-response-body (handler body)
  (wrap-response-mapper handler (lambda (res) (clone-response res :body body))))

(defun wrap-response-body-mapper (handler body-mapper)
  (wrap-response-mapper handler (lambda (res) (funcall body-mapper (response-body res)))))
