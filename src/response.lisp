;;;; response.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.response
  (:use :cl)
  (:export #:responsep
           #:response
           #:make-response
           #:ok
           #:created
           #:accepted
           #:no-content
           #:redirect
           #:bad-request
           #:unauthorized
           #:forbidden
           #:not-found
           #:method-not-allowed
           #:unprocessable-entity
           #:internal-server-error
           #:not-implemented
           #:response-status
           #:response-headers
           #:response-header
           #:response-body
           #:status-response
           #:headers-response
           #:header-response
           #:body-response
           #:headers-response-append
           #:body-mapper-response))

(in-package :tiny-routes.response)

;;; Refinements
(defun responsep (object)
  "Return t if OBJECT is a response."
  (when (listp object)
    (destructuring-bind (status headers body &rest others) object
      (and (null others)
           (and (integerp status) (>= status 100) (< status 600))
           (listp headers)
           ;; Clack expects body to be either a list of strings, a byte vector,
           ;; or a path name
           (typecase body
             (list (every #'stringp body))
             (vector t)
             (pathname t)
             (otherwise nil))))))

(deftype response ()
  '(satisfies responsep))

;;; Constructors
(defun make-response (&key (status 200) headers body)
  "Return a new response with http STATUS, HEADERS, and BODY."
  (list status headers
        (if (stringp body) (list body) body)))

;;; 2xx responses
(defun ok (&key headers body)
  "Return a response with status HTTP 200 OK and BODY."
  (make-response :status 200 :headers headers :body body))

(defun created (location &optional body)
  "Return a response with status HTTP 201 Created with a location header
of LOCATION and BODY."
  (make-response :status 201 :headers (list :location location) :body body))

(defun accepted (&optional body)
  "Return a response with status HTTP 202 Accepted and BODY."
  (make-response :status 202 :body body))

(defun no-content ()
  "Return a response with status HTTP 204 No Content."
  (make-response :status 204))

;;; 3xx responses
(defun redirect (location)
  "Return a response with STATUS with a location header of LOCATION."
  (make-response :status 302 :headers (list :location location)))

;;; 4xx responses
(defun bad-request (&optional body)
  "Return a response with status HTTP 400 Bad Request and BODY."
  (make-response :status 400 :body body))

(defun unauthorized (&optional body)
  "Return a response with status HTTP 401 Unauthorized and BODY."
  (make-response :status 401 :body body))

(defun forbidden (&optional body)
  "Return a response with status HTTP 403 Forbidden and BODY."
  (make-response :status 403 :body body))

(defun not-found (&optional body)
  "Return a response with status HTTP 404 Not Found and BODY."
  (make-response :status 404 :body body))

(defun method-not-allowed (&optional body)
  "Return a response with status HTTP 405 Method Not Allowed and BODY."
  (make-response :status 405 :body body))

(defun unprocessable-entity (&optional body)
  "Return a response with status HTTP 422 Unprocessable Entity and BODY."
  (make-response :status 422 :body body))

;;; 5xx responses
(defun internal-server-error (&optional body)
  "Return a response with status HTTP 500 Internal Server Error and BODY."
  (make-response :status 500 :body body))

(defun not-implemented (&optional body)
  "Return a response with status HTTP 500 Not Implemented and BODY."
  (make-response :status 501 :body body))

;;; Selectors
(defun response-status (response)
  "Return the status of RESPONSE."
  (first response))

(defun response-headers (response)
  "Return the headers of RESPONSE."
  (second response))

(defun response-header (response key &optional default)
  "Return the value of KEY from the headers of RESPONSE."
  (getf (response-headers response) key default))

(defun response-body (response)
  "Return the RESPONSE body.
If response body is a list of a 1 element, return the unwrapped
element."
  (let ((body (third response)))
    (if (and (listp body) (null (cdr body)))
        (car body)
        body)))

;;; Combinators
(defun status-response (response status)
  "Return a clone of RESPONSE with STATUS."
  (make-response :status status
                 :headers (response-headers response)
                 :body (response-body response)))

(defun headers-response (response headers)
  "Return a clone of RESPONSE with HEADERS."
  (make-response :status (response-status response)
                 :headers headers
                 :body (response-body response)))

(defun header-response (response key value)
  "Return a clone of RESPONSE with header KEY and VALUE added."
  (headers-response
   response
   (append (list key value) (response-headers response))))

(defun body-response (response body)
  "Return a clone of RESPONSE with BODY."
  (make-response :status (response-status response)
                 :headers (response-headers response)
                 :body body))

(defun headers-response-append (response header-key header-value)
  "Return a clone of RESPONSE with HEADER-KEY and HEADER-VALUE added."
  (headers-response response (append (list header-key header-value)
                                     (response-headers response))))

(defun body-mapper-response (response body-mapper)
  "Return a clone of RESPONSE where body is mapped via BODY-MAPPER."
  (body-response response (funcall body-mapper (response-body response))))
