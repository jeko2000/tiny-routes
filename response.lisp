;;;; response.lisp
(in-package #:tiny-routes)

;; constructor
(defun make-response (&optional (status 200) headers body)
  "Return a new response with STATUS, HEADERS, and BODY."
  (list status headers (if (listp body)
                           body
                           (list body))))
;; selectors
(defun response-status (response)
  "Return the RESPONSE status."
  (first response))

(defun response-headers (response)
  "Return the RESPONSE headers."
  (second response))

(defun response-body (response)
  "Return the RESPONSE body."
  (third response))

;; predicates
(defun responsep (object)
  "Return t if OBJECT looks like a response."
  (and (listp object)
       (integerp (first object))
       (listp (second object))
       (listp (third object))))

;; util
;; 2xx responses
(defun ok (&optional body)
  "Return a response with status HTTP 200 OK and body BODY."
  (make-response 200 nil body))

(defun created (location &optional body)
  "Return a response with status HTTP 201 Created with a location header
of LOCATION and body BODY."
  (make-response 201 (list :location location) body))

(defun accepted (&optional body)
  "Return a response with status HTTP 202 Accepted and body BODY."
  (make-response 202 nil body))

(defun no-content ()
  "Return a response with status HTTP 204 No Content."
  (make-response 204))

;; 3xx responses
(defun redirect (location &optional (status 302))
  "Return a response with STATUS with a location header of LOCATION."
  (make-response status (list :location location)))

;; 4xx responses
(defun bad-request (&optional body)
  "Return a response with status HTTP 400 Bad Request and body BODY."
  (make-response 400 nil body))

(defun unauthorized (&optional body)
  "Return a response with status HTTP 401 Unauthorized and body BODY."
  (make-response 401 nil body))

(defun forbidden (&optional body)
  "Return a response with status HTTP 403 Forbidden and body BODY."
  (make-response 403 nil body))

(defun not-found (&optional body)
  "Return a response with status HTTP 404 Not Found and body BODY."
  (make-response 404 nil body))

(defun method-not-allowed (&optional body)
  "Return a response with status HTTP 405 Method Not Allowed and body BODY."
  (make-response 405 nil body))

(defun unprocessable-entity (&optional body)
  "Return a response with status HTTP 422 Unprocessable Entity and body BODY."
  (make-response 422 nil body))

;; 5xx responses
(defun internal-server-error (&optional body)
  "Return a response with status HTTP 500 Internal Server Error and body BODY."
  (make-response 500 nil body))

(defun not-implemented (&optional body)
  "Return a response with status HTTP 500 Not Implemented and body BODY."
  (make-response 501 nil body))

;; helpers
(defun status-response (response status)
  "Return a clone of RESPONSE with status STATUS."
  (make-response
   status
   (response-headers response)
   (response-body response)))

(defun header-response (response key value)
  "Return a clone of RESPONSE with header KEY and VALUE added."
  (headers-response
   response
   (plist-append (response-headers response) key value)))

(defun headers-response (response headers)
  "Return a clone of RESPONSE with headers HEADERS."
  (make-response
   (response-status response)
   headers
   (response-body response)))

(defun headers-response-append (response headers)
  "Return a clone of RESPONSE with headers HEADERS added to the existing
headers."
  (make-response
   (response-status response)
   (append headers (response-headers response))
   (response-body response)))

(defun body-response (response body)
  "Return a clone of RESPONSE with body BODY."
  (make-response
   (response-status response)
   (response-headers response)
   body))

(defun body-mapper-response (response body-mapper)
  "Return a clone of RESPONSE where body is mapped via BODY-MAPPER."
  (make-response
   (response-status response)
   (response-headers response)
   (funcall body-mapper (response-body response))))

(defun content-type-response (response content-type)
  "Return a clone of RESPONSE with CONTENT-TYPE."
  (header-response response :content-type content-type))

(defun application/json-response (response)
  "Return a clone of RESPONSE with content-type set to application/json."
  (content-type-response response "application/json"))

(defun text/html-response (response)
  "Return a clone of RESPONSE with content-type set to text/html."
  (content-type-response response "text/html"))
