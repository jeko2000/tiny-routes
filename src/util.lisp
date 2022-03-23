;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.util
  (:use :cl)
  (:export #:pipe
           #:compose))

(in-package :tiny-routes.util)

(defmacro pipe (value &body wrappers)
  "Pipe VALUE through WRAPPERS by subsequently passing each wrapped value as
the first parameter of the next wrapper."
  (reduce
   (lambda (w1 w2)
     (cond ((null w1) w2)
           ((null w2) w1)
           (t
            (let ((w2 (cond ((symbolp w2) `(,w2))
                            (t w2))))
              `(,(car w2) ,w1 ,@(cdr w2))))))
   wrappers :initial-value value))

(defun compose (function &rest other-functions)
  (reduce (lambda (f g)
            (lambda (&rest args)
              (funcall f (apply g args))))
          other-functions
          :initial-value function))
