;;; sbcl --noinform --load adv19-08.lisp [-v n] [-f input-file-name] <-d nnnXmmm>
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *input-file* "adv19-08.input")

(defparameter *dimensions* nil)

;; This is a vector of layers.  Each layer is a vector of rows, each row a list of columns.
(defparameter *input* nil)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-input ()
  (setf *input* (make-array '(1) :fill-pointer 0 :adjustable t))
  (let ((layer 0)
	(row 0)
	(zero-code (char-code #\0))
	(nine-code (char-code #\9)))
    (with-open-file (in *input-file*)
      (do* ((ch (read-char in nil 'the-end) (read-char in nil 'the-end)))
	   ((not (characterp ch)))
	(unless (char= #\Newline ch)
	  (when (< (length *input*) (1+ layer))
	    (vprint 1 "Adding layer ~a~%" layer)
	    (vector-push-extend (make-array (list (second *dimensions*)) :initial-element nil) *input*))
	  (let ((code (char-code ch)))
	    (when (and (>= code zero-code) (<= code nine-code))
	      ;; add a pixel value to the current row
	      (vprint 2 "~a: Adding ~a to ~a~%" layer (- code zero-code) (elt (elt *input* layer) row))
	      (push (- code zero-code) (elt (elt *input* layer) row))
	      (when (= (length (elt (elt *input* layer) row)) (first *dimensions*))
		(incf row))
	      (when (= row (second *dimensions*))
		(setf row 0)
		(incf layer)))))))))

(defun main (args)

  ;; Parse command-line options
  (let ((opts '(("v" :required nil)
		("p" :required nil)
		("d" :required nil)
		("f" :required nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "p" (car arg))
		 (setf *part* (parse-integer (cdr arg))))
		((string= "d" (car arg))
		 (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^(\\d+)[Xx](\\d+)" (cdr arg))
		   (when (not parsedok)
		     (format t "bad dimensions ~a~%" (cdr arg))
		     (sb-ext:exit :code 1))
		   (setf *dimensions* (list (parse-integer (elt vals 0)) (parse-integer (elt vals 1))))))
		((string= "f" (car arg))
		 (setf *input-file* (cdr arg)))))
	(setf args new-args)))

  (when (null *dimensions*)
    (format t "You must specify dimensions for the input (-d nXm)~%")
    (sb-ext:exit :code 1))
  
  (vprint 1 "Dimensions: ~a~%" *dimensions*)
  
  (read-input)

  (vprint 1 "There are ~a layers~%" (length *input*))
  (vprint 2 "input: ~a~%" *input*)

  (let ((min-layer nil)
	(min-layer-sum (* (first *dimensions*) (second *dimensions*))))

    ;; find the layer with the least number of zeroes
    (loop for layer across *input*
       do (let ((num-zeroes (loop for row across layer sum (length (remove-if-not (lambda (n) (= 0 n)) row)))))
	    (when (< num-zeroes min-layer-sum)
	      (setf min-layer-sum num-zeroes)
	      (setf min-layer layer))))
    (vprint 2 "Layer with least number of zeroes is:~%~a~%" min-layer)

    ;; now that we know the layer with the fewest number of zeros,
    ;; (part 1) print the product of the 1-count and the 2-count in that layer.
    (let ((num-ones (loop for row across min-layer sum (length (remove-if-not (lambda (n) (= 1 n)) row))))
	  (num-twos (loop for row across min-layer sum (length (remove-if-not (lambda (n) (= 2 n)) row)))))
      (format t "Result: ~a~%" (* num-ones num-twos))))
	    
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
