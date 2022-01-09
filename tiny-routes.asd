;;;; tiny-routes.asd

(asdf:defsystem :tiny-routes
  :description "A tiny routing library for Common Lisp targeting Clack."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :version "0.1.0"
  :license  "BSD 3-Clause"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :serial t
                :components ((:file "request")
                             (:file "response")
                             (:file "middleware")
                             (:file "tiny-routes"))))
  :in-order-to ((test-op (test-op :tiny-routes/test))))

(asdf:defsystem :tiny-routes/test
  :description "A tiny-routes test suite."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "BSD 3-Clause"
  :depends-on (:tiny-routes :fiveam)
  :pathname "t/"
  :components ((:file "tiny-routes-test"))
  :perform (test-op (o c) (symbol-call :5a :run! :tiny-routes)))
