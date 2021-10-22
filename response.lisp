;;;; response.lisp
(in-package #:tiny-routes)

;; constructor
(defun make-response (status &key headers body)
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
(defun ok (&key headers body)
  "Return a response with status HTTP 200 OK, HEADERS, and BODY."
  (make-response 200 :headers headers :body body))

(defun created (&key headers body)
  "Return a response with status HTTP 201 Created, HEADERS, and BODY."
  (make-response 201 :headers headers :body body))

(defun accepted (&key headers body)
  "Return a response with status HTTP 202 Accepted, HEADERS, and BODY."
  (make-response 202 :headers headers :body body))

(defun no-content (&key headers)
  "Return a response with status HTTP 204 No Content and HEADERS."
  (make-response 204 :headers headers))

;; 3xx responses
(defun found (location)
  "Return a response with status HTTP 302 Found with a location header
of LOCATION."
  (make-response 302 :headers (list :location location)))

(defmacro redirect (location)
  "Alias for `found'."
  `(found ,location))

;; 4xx responses
(defun bad-request (&key headers body)
  "Return a response with status HTTP 204 No Content and HEADERS."
  (make-response 400 :headers headers :body body))

(defun unauthorized (&key headers body)
  "Return a response with status HTTP 401 Unauthorized, HEADERS, and BODY."
  (make-response 401 :headers headers :body body))

(defun forbidden (&key headers body)
  "Return a response with status HTTP 403 Forbidden, HEADERS, and BODY."
  (make-response 403 :headers headers :body body))

(defun not-found (&key headers body)
  "Return a response with status HTTP 404 Not Found, HEADERS, and BODY."
  (make-response 404 :headers headers :body body))

(defun method-not-allowed (&key headers body)
  "Return a response with status HTTP 405 Method Not Allowed, HEADERS, and BODY."
  (make-response 405 :headers headers :body body))

(defun unprocessable-entity (&key headers body)
  "Return a response with status HTTP 422 Unprocessable Entity, HEADERS, and BODY."
  (make-response 422 :headers headers :body body))

;; 5xx responses
(defun internal-server-error (&key headers body)
  "Return a response with status HTTP 500 Internal Server Error, HEADERS, and BODY."
  (make-response 500 :headers headers :body body))

(defun not-implemented (&key headers body)
  "Return a response with status HTTP 500 Not Implemented, HEADERS, and BODY."
  (make-response 501 :headers headers :body body))

;; helpers
(defun status (response status)
  "Return a clone of RESPONSE with status STATUS."
  (make-response
   status
   :headers (response-headers response)
   :body (response-body body)))

(defun headers (response headers)
  "Return a clone of RESPONSE with headers HEADERS."
  (make-response
   (response-status response)
   :headers headers
   :body (response-body body)))

(defun body (response body)
  "Return a clone of RESPONSE with body BODY."
  (make-response
   (response-status response)
   :headers (response-headers response)
   :body body))

(defun header (response key value)
  "Return a clone of RESPONSE with header KEY and VALUE added."
  (headers
   response
   (append (list key value) (response-headers response))))

(defun content-type (response content-type)
  "Return a clone of RESPONSE with CONTENT-TYPE."
  (header response :content-type content-type))

(defun application/json (response)
  "Return a clone of RESPONSE with content-type set to application/json."
  (content-type response "application/json"))

(defun text/html (response)
  "Return a clone of RESPONSE with content-type set to text/html."
  (content-type response "text/html"))
