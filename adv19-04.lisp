;;; sbcl --noinform --load adv19-04.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *verbose* nil)
(defparameter *trace* nil)
(defparameter *part* 1)
(defparameter *input-file* "adv19-04.input")
(defparameter *range* nil)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun has-digit-pair-not-triple (valstr)
  (let ((len (length valstr)))
    (if (< len 3)
	(char= (char valstr 0) (char valstr 1))
	(loop for i upto (- len 2)
	   do (progn
		(if (char= (char valstr i) (char valstr (+ i 1)))
		    (cond ((= i (- len 2))
			   (if (char/= (char valstr i) (char valstr (- i 1)))
			       (return-from has-digit-pair-not-triple t)))
			  ((= i 0)
			   (if (char/= (char valstr i) (char valstr (+ i 2)))
			       (return-from has-digit-pair-not-triple t)))
			  (t
			   (if (and (char/= (char valstr i) (char valstr (+ i 2)))
				    (char/= (char valstr i) (char valstr (- i 1))))
			       (return-from has-digit-pair-not-triple t)))))))))
  nil)

(defun candidate-p (val)
  (let* ((numstr (format nil "~a" val))
	 (sorted (sort (copy-seq numstr) #'char<))
	 (dedup (remove-duplicates (copy-seq numstr)))
	 (len (length numstr))
	 (matches-part1 (and (> len (length dedup))
			     (string= numstr sorted))))
    (if (= *part* 1)
	matches-part1
	(and matches-part1 (has-digit-pair-not-triple sorted)))))

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("v" :required nil)
		("p" :required nil)
		("t" :none nil)
		("r" :required nil)
		("f" :required nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "p" (car arg))
		 (setf *part* (parse-integer (cdr arg))))
		((string= "t" (car arg))
		 (setf *trace* t))
		((string= "r" (car arg))
		 (setf *range* (cdr arg)))
		((string= "f" (car arg))
		 (setf *input-file* (cdr arg)))))
	(setf args new-args)))

  (unless *range*
    (format t "You must provide a range (-r) xx-yy  ~%")
    (sb-ext:exit :code 1))

  (let* ((range (split-sequence:split-sequence #\- *range*))
	 (lbound (parse-integer (first range)))
	 (ubound (parse-integer (second range)))
	 (candidates (make-array '(0) :fill-pointer 0 :adjustable t)))
    (loop for val from lbound to ubound
	 do (when (candidate-p val)
	      (vector-push-extend val candidates)))
    (format t "There are ~a candidates in the range ~a-~a~%" (length candidates) lbound ubound))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
