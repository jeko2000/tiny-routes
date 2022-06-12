;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :tiny-routes.util
  (:use :cl)
  (:export #:pipe
           #:compose
           #:rfc-1123-date))

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

(defvar *day-names*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar *month-names-3*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  (when time
    (multiple-value-bind (second minute hour date month year day-of-week)
        (decode-universal-time time 0)
      (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
              (svref *day-names* day-of-week)
              date
              (svref *month-names-3* (1- month))
              year
              hour
              minute
              second))))
