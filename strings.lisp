(defpackage :dls
  (:use :common-lisp)
  (:export :join :parse-vals))

(in-package :dls)

(defgeneric join (seq sep))
(defmethod join ((l list) separator)
  (with-output-to-string (out)
    (loop for (element . more) on l
	  do (princ element out)
	  when more
	  do (princ separator out))))
(defmethod join ((v vector) separator)
  (join (loop for i across v collect i) separator))

(defun parse-vals (str)
  (with-input-from-string (s str) (loop for x = (read s nil nil) while x collect x)))
