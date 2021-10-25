;;;; tiny-routes.asd

(asdf:defsystem #:tiny-routes
  :description "A tiny routing library for Common Lisp targeting Clack."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :version "0.1.0"
  :license  "BSD 3-Clause"
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "util")
               (:file "request")
               (:file "response")
               (:file "path")
               (:file "middleware")
               (:file "route")))
