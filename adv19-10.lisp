;;; sbcl --noinform --load adv19-10.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)

(defparameter *verbose* nil)
(defparameter *part* 1)
(defparameter *input-file* "adv19-10.input")

(defparameter *map* nil)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-input (filename)
  (let ((l (make-array '(1) :fill-pointer 0 :adjustable t)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
	 while line
	 do (vector-push-extend line l)))
    l))

;; so, here is the outline:
;; read the map
;; for each location with an asteroid:
;;   for each other location with an asteroid:
;;     if there is no intervening asteroid, increment count
;;   attach the count to the asteroid
;; print the highest count, and its position

(defun new-asteroid-enumerator (astmap)
  (let ((row nil)
	(col nil)
	(height nil)
	(width nil))

    (defun asteroid-enumerator ()
      (do ()
	  ((= row height) nil)
	(do ()
	    ((= col width))
	  (incf col)
	  (when (< col width)
	    (if (char= (elt (elt *map* row) col) #\#)
		(return-from asteroid-enumerator (list row col)))))
	(incf row)
	(setf col -1)))

    (setf row 0)
    (setf col -1)
    (setf height (length astmap))
    (setf width (length (elt astmap 0)))
    #'asteroid-enumerator))

;; To find whether an asteroid blocks another:
;;   calculate the delta-x and delta-y between the source and destination asteroids
;;   find the gcd of those
;;   divide each by the gcd
;;   start from the source asteroid and test in (delta-x,delta-y) increments
;;     if you land on an asteroid, return nil
;;     if/when you reach the destination asteroid, return t
(defun no-blocking-asteroids (loc-1 loc-2)
  (vprint 1 "    no-blocking-asteroids ~a ~a~%" loc-1 loc-2)
  (let* ((row-1 (first loc-1))
	 (col-1 (second loc-1))
	 (row-2 (first loc-2))
	 (col-2 (second loc-2))
	 (delta-row (- row-1 row-2))
	 (delta-col (- col-1 col-2))
	 (g (gcd (abs delta-row) (abs delta-col)))
	 (inc-row (/ delta-row g))
	 (inc-col (/ delta-col g)))
    (vprint 2 "    dr ~a dc ~a g ~a ir ~a ic ~a~%" delta-row delta-col g inc-row inc-col)
    (do ((row (+ row-2 inc-row) (incf row inc-row))
	 (col (+ col-2 inc-col) (incf col inc-col)))
	((and (= row row-1) (= col col-1)) (vprint 2 "      Clear!~%") t)
      (vprint 2 "    Step ~a,~a~%" row col)
      (when (char= (elt (elt *map* row) col) #\#)
	(vprint 2 "      HIT!~%")
	(return-from no-blocking-asteroids nil)))))

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

  (vprint 1 "Reading map~%")
  (setf *map* (read-input *input-file*))

  (vprint 1 "map: ~a~%" *map*)

  (let ((max-count 0)
	(max-count-location nil)
	(ast-iter-outer (new-asteroid-enumerator *map*)))
    ;; consider the visible-count of each asteroid
    (do ((my-coords (funcall ast-iter-outer) (funcall ast-iter-outer)))
	((null my-coords))
      (vprint 1 "~%===================~%Outer ~a~%" my-coords)
      (let ((my-count 0)
	    (ast-iter-inner (new-asteroid-enumerator *map*)))
	;; Consider the visibility of every other asteroid
	(do ((their-coords (funcall ast-iter-inner) (funcall ast-iter-inner)))
	    ((null their-coords))
	  (vprint 1 "~%  Inner ~a~%" their-coords)
	  (when (not (equal my-coords their-coords))
	    (when (no-blocking-asteroids my-coords their-coords)
	      (incf my-count))))
	(vprint 1 "  Count for ~a is ~a~%" my-coords my-count)
	(when (> my-count max-count)
	  (vprint 1 "  NEW WINNER!~%")
	  (setf max-count my-count)
	  (setf max-count-location my-coords))))
    (format t "Max visibility count is ~a at ~a~%" max-count max-count-location))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
