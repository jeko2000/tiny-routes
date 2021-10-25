;;;; util.lisp
(in-package #:tiny-routes)

(defun plist-append (plist key value)
  "Return a clone of PLIST containing KEY and VALUE."
  (append (list key value) plist))

(defun read-stream-to-string (input-stream)
  "Read INPUT-STREAM entirely and return the contents as a string."
  (with-output-to-string (output-stream)
    (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
      (loop for pos = (read-sequence buf input-stream)
            while (plusp pos)
            do (write-sequence buf output-stream :end pos)))))
