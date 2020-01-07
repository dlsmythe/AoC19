;;; sbcl --noinform --load adv19-12.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(load "strings.lisp")

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *input-file* "adv19-12.input")

(defparameter *moonv* (make-array '(1) :adjustable t :fill-pointer 0))

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

;; ===========================================

(defstruct moon
  (x 0 )
  (y 0 )
  (z 0 )
  (dx 0)
  (dy 0)
  (dz 0))

(defun add-moon (x y z)
  (let ((m (make-moon :x x :y y :z z)))
    (vector-push-extend m *moonv*)))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
       while line
       do (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>" line)
	    (vprint 1 "vals: ~a~%" vals)
	    (when (not parsedok)
	      (vprint 1 "bad moon: ~a ~%" line)
	      (sb-ext:exit :code 1))
	    (add-moon
	     (parse-integer (elt vals 0))
	     (parse-integer (elt vals 1))
	     (parse-integer (elt vals 2)))))))

(defun total-energy (state)
  (loop for i below (length state)
     sum (let ((moon (elt state i)))
	   (* (+ (abs (moon-x moon))
		 (abs (moon-y moon))
		 (abs (moon-z moon)))
	      (+ (abs (moon-dx moon))
		 (abs (moon-dy moon))
		 (abs (moon-dz moon)))))))

(defun do-one-timestep (state)
  (let ((num-moons (length state)))
    ;; first, update the velocities
    (loop for i below num-moons
       do (let ((state-i (elt state i)))
	    (loop for j below num-moons
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
    (loop for i below num-moons
       do (let ((state-i (elt state i)))
	    (incf (moon-x state-i) (moon-dx state-i))
	    (incf (moon-y state-i) (moon-dy state-i))
	    (incf (moon-z state-i) (moon-dz state-i))))))

(defmacro same-on-axis (A DA)
  `(do ((done nil)
	(result t)
	(i 0 (1+ i)))
       ((or done (= i (length *moonv*))) result)
     (unless (and (equalp (,A (elt initial-state i)) (,A (elt *moonv* i)))
		  (equalp (,DA (elt initial-state i)) (,DA (elt *moonv* i))))
       (setf done t)
       (setf result nil))))
  
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
    (let ((initial-state (make-array (list (length *moonv*))))
	  (state-index 1)
	  (x-cycle 0)
	  (y-cycle 0)
	  (z-cycle 0))
      (dotimes (i (length *moonv*))
	(setf (elt initial-state i) (copy-structure (elt *moonv* i))))
      (do-one-timestep *moonv*)
      (incf state-index)
      (do ()
	  ((and (/= 0 x-cycle) (/= 0 y-cycle) (/= 0 z-cycle)))
	(when (and (= 0 x-cycle) (same-on-axis moon-x moon-dx))
	  (vprint 1 "X cycle at iteration ~a~%" state-index)
	  (setf x-cycle state-index))
	(when (and (= 0 y-cycle) (same-on-axis moon-y moon-dy))
	  (vprint 1 "Y cycle at iteration ~a~%" state-index)
	  (setf y-cycle state-index))
	(when (and (= 0 z-cycle) (same-on-axis moon-z moon-dz))
	  (vprint 1 "Z cycle at iteration ~a~%" state-index)
	  (setf z-cycle state-index))
	(incf state-index)
	(do-one-timestep *moonv*))

      (format t "Cycle is LCM(~a,~a,~a) = ~a~%" x-cycle y-cycle z-cycle
	      (lcm (1- x-cycle) (lcm (1- y-cycle) (1- z-cycle))))))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
