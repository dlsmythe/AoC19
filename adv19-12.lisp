;;; sbcl --noinform --load adv19-11.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)
(ql:quickload "md5" :silent t)		; for (md5:md5sum-string)

(load "strings.lisp")

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *input-file* "adv19-12.input")

(defparameter *moonv* (make-array '(1) :adjustable t :fill-pointer 0))
(defparameter *max-seen* nil)
(defparameter *min-seen* nil)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

;; ===========================================

(defstruct moon
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  (dx 0 :type fixnum)
  (dy 0 :type fixnum)
  (dz 0 :type fixnum))

(defun add-moon (x y z)
  (let ((m (make-moon :x x :y y :z z)))
    (vector-push-extend m *moonv*)))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
       while line
       do (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>" line)
	    (format t "vals: ~a~%" vals)
	    (when (not parsedok)
	      (format t "bad moon: ~a ~%" line)
	      (sb-ext:exit :code 1))
	    (add-moon
	     (parse-integer (elt vals 0))
	     (parse-integer (elt vals 1))
	     (parse-integer (elt vals 2)))))))

(defun total-energy (state)
  (declare (ftype (function () fixnum) total-energy))
  (loop for i fixnum below (length state)
     sum (let ((moon (elt state i)))
	   (* (+ (abs (moon-x moon))
		 (abs (moon-y moon))
		 (abs (moon-z moon)))
	      (+ (abs (moon-dx moon))
		 (abs (moon-dy moon))
		 (abs (moon-dz moon)))))))

(defun update-extrema (state)
  (loop for moon across state
     do (let ((x (moon-x moon))
	      (y (moon-y moon))
	      (z (moon-z moon)))
	  (if (> x (moon-x *max-seen*))
	      (setf (moon-x *max-seen*) x))
	  (if (> y (moon-y *max-seen*))
	      (setf (moon-y *max-seen*) y))
	  (if (> z (moon-z *max-seen*))
	      (setf (moon-z *max-seen*) z))
	  (if (< x (moon-x *min-seen*))
	      (setf (moon-x *min-seen*) x))
	  (if (< y (moon-y *min-seen*))
	      (setf (moon-y *min-seen*) y))
	  (if (< z (moon-z *min-seen*))
	      (setf (moon-z *min-seen*) z)))))

(defun do-one-timestep (state)
  (update-extrema state)
  (let ((num-moons (length state)))
    (declare (fixnum num-moons))
    ;; first, update the velocities
    (loop for i fixnum below num-moons
       do (let ((state-i (elt state i)))
	    (loop for j fixnum below num-moons
	       do (let ((state-j (elt state j)))
		    (when (/= i j)
		      (let ((us-x (moon-x state-i))
			    (us-y (moon-y state-i))
			    (us-z (moon-z state-i))
			    (them-x (moon-x state-j))
			    (them-y (moon-y state-j))
			    (them-z (moon-z state-j)))
			(when (/= us-x them-x)
			  (incf (moon-dx state-i) (if (> (- us-x them-x) 0) -1 1)))
			(when (/= us-y them-y)
			  (incf (moon-dy state-i) (if (> (- us-y them-y) 0) -1 1)))
			(when (/= us-z them-z)
			  (incf (moon-dz state-i) (if (> (- us-z them-z) 0) -1 1)))))))))
    ;; Now, update the positions
    (loop for i fixnum below num-moons
       do (let ((state-i (elt state i)))
	    (incf (moon-x state-i) (moon-dx state-i))
	    (incf (moon-y state-i) (moon-dy state-i))
	    (incf (moon-z state-i) (moon-dz state-i))))))

(defun state-stamp (state)
  (declare (ftype (function (vector) string) state-stamp))
  ;; (dls:join
  ;;  (loop for i below (length state)
  ;;     collect (format nil "~a ~a ~a ~a ~a ~a"
  ;; 		      (moon-x (elt state i))
  ;; 		      (moon-y (elt state i))
  ;; 		      (moon-z (elt state i))
  ;; 		      (moon-dx (elt state i))
  ;; 		      (moon-dy (elt state i))
  ;; 		      (moon-dz (elt state i))))
  ;;  "|"))
  (let* ((s-0 (elt state 0))
	 (s-1 (elt state 1))
	 (s-2 (elt state 2))
	 (s-3 (elt state 3))
	 (m0 (concatenate 'string
			  (write-to-string (moon-x s-0)) " "
			  (write-to-string (moon-y s-0)) " "
			  (write-to-string (moon-z s-0)) "/"
			  (write-to-string (moon-dx s-0)) " "
			  (write-to-string (moon-dy s-0)) " "
			  (write-to-string (moon-dz s-0))))
	 (m1 (concatenate 'string
			  (write-to-string (moon-x s-1)) " "
			  (write-to-string (moon-y s-1)) " "
			  (write-to-string (moon-z s-1)) "/"
			  (write-to-string (moon-dx s-1)) " "
			  (write-to-string (moon-dy s-1)) " "
			  (write-to-string (moon-dz s-1))))
	 (m2 (concatenate 'string
			  (write-to-string (moon-x s-2)) " "
			  (write-to-string (moon-y s-2)) " "
			  (write-to-string (moon-z s-2)) "/"
			  (write-to-string (moon-dx s-2)) " "
			  (write-to-string (moon-dy s-2)) " "
			  (write-to-string (moon-dz s-2))))
	 (m3 (concatenate 'string
			  (write-to-string (moon-x s-3)) " "
			  (write-to-string (moon-y s-3)) " "
			  (write-to-string (moon-z s-3)) "/"
			  (write-to-string (moon-dx s-3)) " "
			  (write-to-string (moon-dy s-3)) " "
			  (write-to-string (moon-dz s-3)))))
    (concatenate 'string m0 "|"  m1 "|"  m2 "|"  m3)))

(defun states= (s0 s1)
  ;; (string= (state-stamp s0) (state-stamp s1))
  ;;(reduce (lambda (l r) (and l r)) (loop for i fixnum below (length s0) collect (equalp (elt s0 i) (elt s1 i)))))
  (loop for i fixnum below (length s0)
     do (unless (equalp (elt s0 i) (elt s1 i))
	  (return-from states= nil)))
  t)

(defun main (args)
  
  ;; Parse command-line options
  (let ((opts '(("v" :required nil)
		("p" :required nil)
		("f" :required nil))))
    (multiple-value-bind (new-args vals) (getopt:getopt args opts)
      (dolist (arg vals)
	(cond ((string= "v" (car arg))
	       (setf *verbose* (parse-integer (cdr arg))))
	      ((string= "p" (car arg))
	       (setf *part* (parse-integer (cdr arg))))
	      ((string= "f" (car arg))
	       (setf *input-file* (cdr arg)))))
      (setf args new-args)))

  (read-input *input-file*)

  (when (= *part* 1)
    (let ((state (make-array (list (length *moonv*)))))
      (dotimes (i (length *moonv*))
	(setf (elt state i) (copy-structure (elt *moonv* i))))
      (dotimes (i 1000)
	(do-one-timestep state))
      (format t "Final total energy: ~a~%" (total-energy state))))

  (when (= *part* 2)
    (let ((slow-state (make-array (list (length *moonv*))))
	  (fast-state (make-array (list (length *moonv*))))
	  (slow-index 0)
	  (fast-index 1))
      (declare (fixnum slow-index fast-index))
      (setf *max-seen* (make-moon))
      (setf *min-seen* (make-moon))
      (dotimes (i (length *moonv*))
	(setf (elt slow-state i) (copy-structure (elt *moonv* i)))
	(setf (elt fast-state i) (copy-structure (elt *moonv* i))))
      (do-one-timestep fast-state)
      (do ()
	  ((states= slow-state fast-state))
	(when (> *verbose* 0)
	  (when t
;;	  (when (= (mod fast-index 1000000) 0)
	    (vprint 1 "Fast ~:d:~%" fast-index)
	    (dotimes (j (length fast-state))
	      (vprint 1 "  (~a,~a,~a)/(~a,~a,~a)~%"
		      (moon-x (elt fast-state j))
		      (moon-y (elt fast-state j))
		      (moon-z (elt fast-state j))
		      (moon-dx (elt fast-state j))
		      (moon-dy (elt fast-state j))
		      (moon-dz (elt fast-state j))))))
	(when (= (mod fast-index 2) 0)
	  (incf slow-index)
	  (do-one-timestep slow-state))
	(incf fast-index)
	(do-one-timestep fast-state))

      (format t "Same state period is ~a: ~a/~a~%Fast-State: ~a~%Slow-State: ~a~%" (- fast-index slow-index) fast-index slow-index fast-state slow-state)))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
