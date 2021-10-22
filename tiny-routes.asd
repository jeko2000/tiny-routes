;;;; tiny-routes.asd

(asdf:defsystem #:tiny-routes
  :description "A tiny routing library for Common Lisp targeting Clack."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :version "0.0.1"
  :license  "BSD 3-Clause"
  :serial t
  :components ((:file "package")
               (:file "response")))
