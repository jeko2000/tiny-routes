;;;; tiny-routes-middleware-cookie.asd

(asdf:defsystem :tiny-routes-middleware-cookie
  :description "A tiny routing middleware for HTTP cookies."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :version "0.1.1"
  :license  "BSD 3-Clause"
  :serial t
  :depends-on (:tiny-routes :cl-cookie)
  :pathname "src/"
  :components ((:module "middleware"
                :components ((:file "cookie"))))
  :in-order-to ((test-op (test-op :tiny-routes-middleware-cookie/test))))

(asdf:defsystem :tiny-routes-middleware-cookie/test
  :description "A routing system for middleware-cookie."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "BSD 3-Clause"
  :depends-on (:tiny-routes-middleware-cookie :fiveam)
  :pathname "t/"
  :components ((:file "tiny-routes-middleware-cookie-test"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :tiny-routes-middleware-cookie-test)))
