;;;; request-body.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.request-body
  (:use :cl)
  (:import-from :tiny-routes.request
                #:raw-body
                #:content-length
                #:request-append)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-request-mapper)
  (:import-from :tiny-routes.response
                #:payload-too-large)
  (:export #:read-stream-to-string
           #:input-stream-mapper
           #:payload-too-large-handler
           #:wrap-request-body))

(in-package :tiny-routes.middleware.request-body)

(defun read-stream-to-string (input-stream content-length)
  "Read INPUT-STREAM return contents as a string.

If CONTENT-LENGTH is non-nil, it must be a positive integer
representing the maximum number of bytes to read from INPUT-STREAM."
  (with-output-to-string (output-stream)
    (if content-length
        (let* ((buffer (make-array content-length :element-type (stream-element-type input-stream)))
               (position (read-sequence buffer input-stream)))
          (write-sequence buffer output-stream :end position))
        (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
          (loop for pos = (read-sequence buf input-stream)
                while (plusp pos)
                do (write-sequence buf output-stream :end pos))))))

(defun payload-too-large-handler (request)
  "Return a response representing a payload too large to be handled."
  (declare (ignore request))
  (payload-too-large))

(defun wrap-request-body (handler &key max-bytes
                                    (input-stream-mapper #'read-stream-to-string)
                                    (payload-too-large-handler #'payload-too-large-handler))
  "Wrap HANDLER such that the result of applying INPUT-STREAM-MAPPER to
the request body is made available to the request via `:request-body'.

If MAX-BYTES is non-nil, it must be a positive integer representing the
maximum number of bytes to read from INPUT-STREAM.

PAYLOAD-TOO-LARGE-HANDLER is a `handler' called if MAX-BYTES is non nil
and the content-length of string is greater than MAX-BYTES."
  (lambda (request)
    (let ((content-length (content-length request)))
      (cond ((null content-length)
             (funcall handler request))
            ;; handle if request is too large
            ((and (typep max-bytes '(integer 1))
                  (> content-length max-bytes))
             (funcall payload-too-large-handler request))
            (t
             (let ((body (funcall input-stream-mapper (raw-body request) content-length)))
               (funcall handler (request-append request :request-body body))))))))
