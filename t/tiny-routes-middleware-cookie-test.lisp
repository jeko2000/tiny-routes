;;;; tiny-routes-middleware-cookie-test.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes-middleware-cookie-test
  (:use :cl :fiveam :tiny-routes.middleware.cookie))

(in-package :tiny-routes-middleware-cookie-test)

(def-suite* :tiny-routes-middleware-cookie-test
  :description "The top level cookie middleware test.")

(defvar *cookie1* (cookie:make-cookie :name "name1" :value "value1"))
(defvar *cookie2* (cookie:make-cookie :name "name2" :value "value2"))

(defvar *cookie3*
  (cookie:make-cookie
   :name "name3" :value "value3"
   :expires (encode-universal-time 6 22 19 25 1 2002 0)
   :path "/"
   :domain "example.com"
   :secure-p t
   :httponly-p nil))

(defvar *cookie4*
  (cookie:make-cookie
   :name "name4" :value "value4"
   :expires (encode-universal-time 6 22 19 25 1 2002 0)
   :path "/"
   :domain "example.com"
   :secure-p nil
   :httponly-p t))

(defvar *cookie-header* "name1=value1;name2=value2")

(test parse-cookie-header-tests
  (is (cookie:cookie= *cookie1* (first (parse-cookie-header *cookie-header*))))
  (is (cookie:cookie= *cookie2* (second (parse-cookie-header *cookie-header*)))))

(defun echo-handler (request)
  request)

(defun mock-request (&key cookies)
  (let ((headers (make-hash-table :test 'equal)))
    (when cookies
      (setf (gethash "cookie" headers)
            (cl-cookie:write-cookie-header cookies)))
    (tiny:make-request :headers headers)))

(test wrap-request-cookies-tests
  (let* ((mock-request (mock-request :cookies (list *cookie1* *cookie2*)))
         (handler (wrap-request-cookies #'echo-handler))
         (echo (funcall handler mock-request)))
    (is (cookie:cookie= *cookie1* (first (tiny:request-get echo :cookies))))
    (is (cookie:cookie= *cookie2* (second (tiny:request-get echo :cookies))))))

(defun handler (request)
  (declare (ignore request))
  (tiny:ok "TEST"))

(test wrap-response-cookies-tests
  (let* ((mock-request (mock-request))
         (handler (wrap-response-cookies #'handler (list *cookie3* *cookie4*)))
         (response (funcall handler mock-request))
         (headers (tiny:response-headers response)))
    (is (= 4 (length headers)))
    (is (string= "name3=value3; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Path=/; Domain=example.com; Secure"
                 (second headers)))
    (is (string= "name4=value4; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Path=/; Domain=example.com; HttpOnly"
                 (fourth headers)))))
