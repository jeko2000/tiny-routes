;;;; tiny-routes.asd

(asdf:defsystem :tiny-routes
  :description "A tiny routing library for Common Lisp targeting Clack."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :version "0.1.1"
  :license  "BSD 3-Clause"
  :serial t
  :depends-on (:cl-ppcre :uiop)
  :pathname "src/"
  :depends-on (:cl-ppcre)
  :components ((:file "util")
               (:file "request")
               (:file "response")
               (:module "middleware"
                :serial t
                :components ((:file "builder")
                             (:file "method")
                             (:file "path-template")
                             (:file "query-parameters")
                             (:file "request-body")
                             (:file "response")
                             (:file "middleware")))
               (:file "tiny-routes"))
  :in-order-to ((test-op (test-op :tiny-routes/test))))

(asdf:defsystem :tiny-routes/test
  :description "A tiny-routes test suite."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "BSD 3-Clause"
  :depends-on (:tiny-routes :fiveam)
  :pathname "t/"
  :components ((:file "tiny-routes-test"))
  :perform (test-op (o c) (symbol-call :tiny-routes-test :run-tests)))
