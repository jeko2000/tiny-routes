;;;; tiny-routes-test.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes-test
  (:use :cl :tiny-routes :fiveam))

(in-package :tiny-routes-test)

(def-suite tiny-routes
  :description "The top-level testing suite for tiny-routes.")

(defun run-tests ()
  (run! 'tiny-routes))

(def-suite* request :in tiny-routes)

(test request-test
  (let* ((request (make-request
                   :request-uri "/home" :request-method :post
                   :url-scheme "https" :path-parameters '(:key "value")
                   :content-type "text/plain" :foo 42))
         (request2 (request-append request :bar :baz)))
    (is (requestp request))
    (is (string= "/home" (uri request)))
    (is (string= "/home" (path-info request)))
    (is (eq :post (request-method request)))
    (is (string= "https" (url-scheme request)))
    (is (string= "value" (getf (request-get request :path-parameters) :key)))
    (is (string= "text/plain" (content-type request)))
    (is (= 42 (request-get request :foo)))
    (is (eq :baz (request-get request2 :bar)))
    (is (eq :default (request-get request2 :unknown :default)))))

(def-suite* response :in tiny-routes)

(test response-test0
  (let ((valid0 (created "/" "Created"))
        (valid1 (make-response :status 404 :body #P"/"))
        (valid2 (make-response :body '("A" "valid" "response")))
        (invalid0 (make-response :body '("Invalid" 'response)))
        (async (lambda (responder) (funcall responder (make-response)))))
    (is (responsep valid0))
    (is (responsep valid1))
    (is (responsep valid2))
    (is (responsep async))
    (is (not (responsep invalid0)))
    (is (string= "Created" (response-body valid0)))
    (is (= 201 (response-status valid0)))
    (is (= 404 (response-status valid1)))
    (is (equalp '("A" "valid" "response") (response-body valid2)))))

(test response-test1
  (let* ((response (make-response))
         (response (status-response response 201))
         (response (headers-response response '(:content-type "text/plain")))
         (response (header-response response :server "test"))
         (response (body-response response "created"))
         (response (body-mapper-response response #'string-upcase)))
    (is (= 201 (response-status response)))
    (is (= 4 (length (response-headers response))))
    (is (string= "text/plain" (response-header response :content-type)))
    (is (string= "test" (response-header response :server)))
    (is (string= "CREATED" (response-body response)))))

(def-suite* middleware :in tiny-routes)

(defun get-request-p (request)
  (eq :get (request-method request)))

(defun post-request-p (request)
  (eq :post (request-method request)))

(test middleware-test0
  (let* ((request (make-request :request-uri "/home" :request-method :get))
         (response (make-response :status 200 :body "OK"))
         (handler (lambda (request)
                    (declare (ignore request))
                    response)))
    (is (equalp response (funcall handler request)))
    (is (equalp response (funcall (wrap-request-matches-method handler :get) request)))
    (is (null (funcall (wrap-request-matches-method handler :post) request)))
    (is (equalp response (funcall (wrap-request-matches-path-template handler "/:page") request)))
    (is (null (funcall (wrap-request-matches-path-template handler "/two/:level") request)))))

(defun echo-handler (request)
  (make-response :status 200 :body request))

(test middleware-test1
  (with-input-from-string (in "Sample request body stream")
    (let* ((request (make-request :request-method :get :request-uri "/users/jeko" :raw-body in
                                  :content-length 26)))
      (is (eq in (raw-body request)))
      (is (string= "Sample request body stream"
                   (request-get (response-body (funcall (wrap-request-body #'echo-handler) request))
                                :request-body)))
      (is (equalp '(413 nil nil)
                  (funcall (wrap-request-body #'echo-handler :max-bytes 2) request)))
      (is (equalp '(:user-id "jeko")
                  (request-get (response-body
                                (funcall
                                 (wrap-request-matches-path-template #'echo-handler "/users/:user-id")
                                 request))
                               :path-parameters))))))

(def-suite* routes :in tiny-routes)

(defun mock-request (method uri &optional body)
  (let ((request (make-request :request-method method
                               :request-uri uri)))
    (if body
        (with-input-from-string (in body)
          (request-append request :raw-body in))
        request)))

(test route-matching0
  (let ((request (mock-request :post "/foo"))
        (response (make-response :status 200 :body "OK")))
    (is (null (funcall (define-get "/foo" () response) request)))
    (is (null (funcall (define-put "/foo" () response) request)))
    (is (null (funcall (define-delete "/foo" () response) request)))
    (is (null (funcall (define-post "/bar" () response) request)))
    (is (equalp response (funcall (define-post "/foo" () response) request)))
    (is (equalp response (funcall (define-post "/:foo" () response) request)))))
