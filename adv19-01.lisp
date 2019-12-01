;;; Run with: sbcl --noinform --load adv19-01.lisp < adv19-01.input
;;;
;;; In this one, I was getting reacquainted.  New things:
;;; - read a file into a list
;;; - arg parsing
;;; - vprint macro
;;; - loop/collect, map, reduce

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)

(defparameter *verbose* 0)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-masses ()
  (nreverse
    (loop for line = (read-line *STANDARD-INPUT* nil)
       while line
       collect (parse-integer line))))

(defun fuel-needed (mass)
  (let ((fuel-mass (- (truncate (/ mass 3)) 2)))
    (vprint 1 "Fuel needed for mass ~A: ~A~%" mass (if (<= fuel-mass 0) 0 fuel-mass))
    (if (<= fuel-mass 0)
	  0
	  (+ fuel-mass (fuel-needed fuel-mass)))))

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("v" :required 0))))
    (multiple-value-bind (new-args vals) (getopt:getopt args opts)
      (dolist (arg vals)
	(cond ((string= "v" (car arg))
	       (setf *verbose* (parse-integer (cdr arg))))))
      (setf args new-args)))

  (let ((masses (read-masses)))
    (format t "Total: ~A~%" (reduce #'+ (map 'list #'fuel-needed masses))))
  0)

(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
