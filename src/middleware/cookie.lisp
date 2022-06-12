;;;; cookie.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.middleware.cookie
  (:use :cl)
  (:import-from :tiny-routes.middleware.builder
                #:wrap-request-mapper)
  (:import-from :tiny-routes.request
                #:request-header
                #:request-append)
  (:import-from :tiny-routes.response
                #:clone-response
                #:response-headers)
  (:local-nicknames (:util :tiny-routes.util))
  (:export #:*rfc-2616-separators*
           #:*rfc-2616-token*
           #:*re-cookie-header*
           #:write-set-cookie-header
           #:parse-cookie-header
           #:wrap-request-cookies
           #:write-response-cookies
           #:wrap-response-cookies))

(in-package :tiny-routes.middleware.cookie)

(defvar *rfc-2616-separators*
  '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\/ #\[ #\] #\? #\= #\{ #\} #\Space #\Tab)
  "RFC 2616 separators as defined in Section 2.2.")

(defvar *rfc-2616-token*
  (with-output-to-string (s)
    (write-char #\[ s)
    (loop for i from 33 below 127
          for char = (code-char i)
          when (and (not (alphanumericp char))
                    (not (member char *rfc-2616-separators*)))
            do (write-char char s))
    (write-string "A-Za-z0-9]+" s))
  "RFC 2616 token as defined in Section 2.2.")

(defvar *re-cookie-header*
  (ppcre:create-scanner
   (format nil "\\s*(~A)=(~A)\\s*[,;]?" *rfc-2616-token* *rfc-2616-token*))
  "A CL-PPCRE scanner matching a key/value cookie as present in an HTTP
Cookie header.")

(defun write-set-cookie-header (cookie &optional stream)
  "Write COOKIE to STREAM as the value of an HTTP Set-Cookie header."
  (format stream
          "~A=~A~@[; Expires=~A~]~@[; Path=~A~]~@[; Domain=~A~]~:[~;; Secure~]~:[~;; HttpOnly~]"
          (cl-cookie:cookie-name cookie)
          (cl-cookie:cookie-value cookie)
          (util:rfc-1123-date (cl-cookie:cookie-expires cookie))
          (cl-cookie:cookie-path cookie)
          (cl-cookie:cookie-domain cookie)
          (cl-cookie:cookie-secure-p cookie)
          (cl-cookie:cookie-httponly-p cookie)))

(defun parse-cookie-header (cookie-header)
  "Return list of cookies as parsed from the COOKIE-HEADER."
  (let (cookies)
    (ppcre:do-register-groups (name value) (*re-cookie-header* cookie-header)
      (when (and (stringp name) (stringp value))
        (push (cl-cookie:make-cookie :name name :value value) cookies)))
    (nreverse cookies)))

(defun wrap-request-cookies (handler &key (cookie-header-parser #'parse-cookie-header))
  "Wrap HANDLER such that the results of applying COOKIE-HEADER-PARSER
to the value of the HTTP Cookie header is made available to the
request via `:cookies'."
  (wrap-request-mapper
   handler
   (lambda (request)
     (let* ((cookie-header-value (request-header request "cookie" ""))
            (cookies (funcall cookie-header-parser cookie-header-value)))
       (request-append request :cookies cookies)))))

(defun write-response-cookies (response cookies)
  "Wrap RESPONSE such that each cookie in COOKIES is written as an HTTP
Set-Cookie header."
  (let* ((cookies (if (consp cookies) cookies (cons cookies nil)))
         (set-cookie-headers (alexandria:mappend
                              #'(lambda (c) (list :set-cookie (write-set-cookie-header c)))
                              cookies)))
    (clone-response response
                    :headers (append set-cookie-headers (response-headers response)))))

(defun wrap-response-cookies (handler cookies)
  "Wrap HANDLER such that the response contains HTTP Set-Cookie headers
corresponding to COOKIES."
  (tiny:wrap-response-mapper
   handler
   (lambda (response)
     (write-response-cookies response cookies))))
