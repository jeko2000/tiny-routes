;;;; request-body.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.request-body
  (:use :cl)
  (:import-from :tiny-routes.request
                #:raw-body
                #:request-append)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-request-mapper)
  (:export #:read-stream-to-string
           #:input-stream-mapper
           #:wrap-request-body))

(in-package :tiny-routes.middleware.request-body)

;; TODO: Add support for a max number of bytes the server is willing
;; to read before sending back a Payload Too Large response.
(defun read-stream-to-string (input-stream)
  "Read INPUT-STREAM entirely and return the contents as a string."
  (when input-stream)
  (with-output-to-string (output-stream)
    (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
      (loop for pos = (read-sequence buf input-stream)
            while (plusp pos)
            do (write-sequence buf output-stream :end pos)))))

(defun wrap-request-body (handler &key (input-stream-mapper #'read-stream-to-string))
  "Wrap HANDLER such that the result of applying INPUT-STREAM-MAPPER to
the request body is made available to the request via `:request-body'."
  (wrap-request-mapper
   handler
   (lambda (request)
     (let ((body (funcall input-stream-mapper (raw-body request))))
       (request-append request :request-body body)))))
