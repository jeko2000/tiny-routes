;;;; package.lisp

(defpackage #:tiny-routes
  (:nicknames :tiny)
  (:use #:cl)
  (:export
   ;; util
   #:pipe
   #:read-stream-to-string
   ;; request
   #:request-append
   #:request-body
   #:request-clack.io
   #:request-clack.streaming
   #:request-content-length
   #:request-content-type
   #:request-get
   #:request-header
   #:request-headers
   #:request-method
   #:request-path-info
   #:request-path-param
   #:request-path-params
   #:request-query-string
   #:request-raw-body
   #:request-remote-address
   #:request-remote-port
   #:request-script-name
   #:request-server-name
   #:request-server-port
   #:request-server-protocol
   #:request-uri
   #:request-url-scheme
   #:with-request-slots
   ;; response
   #:accepted
   #:application/json-response
   #:bad-request
   #:body-mapper-response
   #:body-response
   #:content-type-response
   #:created
   #:forbidden
   #:header-response
   #:headers-response
   #:headers-response-append
   #:internal-server-error
   #:make-response
   #:method-not-allowed
   #:no-content
   #:not-found
   #:not-implemented
   #:ok
   #:redireect
   #:response-body
   #:response-headers
   #:response-status
   #:responsep
   #:status-response
   #:text/html-response
   #:unauthorized
   #:unprocessable-entity
   ;; path
   #:*path-token-regex*
   #:*path-token-scanner*
   #:make-path-template-matcher
   #:make-regex-path-matcher
   ;; middleware
   #:wrap-middleware
   #:wrap-post-match-middleware
   #:wrap-request-body
   #:wrap-request-mapper
   #:wrap-request-matches-method
   #:wrap-request-matches-path-template
   #:wrap-request-predicate
   #:wrap-response-body
   #:wrap-response-content-type
   #:wrap-response-header
   #:wrap-response-headers
   #:wrap-response-headers-append
   #:wrap-response-mapper
   #:wrap-response-mapper*
   #:wrap-response-status
   ;; route
   #:define-routes
   #:route
   #:route-any
   #:route-delete
   #:route-get
   #:route-head
   #:route-options
   #:route-post
   #:route-put
   #:routes))
