;;;; util.lisp
(in-package #:tiny-routes)

(defmacro pipe (value &body wrappers)
  "Pipe VALUE through WRAPPERS by subsequently passing each wrapped
value as the first parameter of the next wrapper."
  (reduce
   (lambda (w1 w2)
     (cond ((null w1) w2)
           ((null w2) w1)
           (t
            (let ((w2 (cond ((symbolp w2) `(,w2))
                            (t w2))))
              `(,(car w2) ,w1 ,@(cdr w2))))))
   wrappers :initial-value value))

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

(defun compose (function &rest other-functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          other-functions
          :initial-value function))
