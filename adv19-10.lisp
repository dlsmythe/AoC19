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
(defparameter *rotation-granularity* 5000)
(defparameter *ray-epsilon* .2)	; distance
(defparameter *proximity-epsilon* .2)

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

(defstruct ast-list-elem
  location
  distance
  angle)

;; To find whether an asteroid blocks another:
;;   calculate the delta-x and delta-y between the source and destination asteroids
;;   find the gcd of those
;;   divide each by the gcd
;;   start from the source asteroid and test in (delta-x,delta-y) increments
;;     if you land on an asteroid, return nil
;;     if/when you reach the destination asteroid, return t
(defun first-blocking-asteroid (loc-1 loc-2)
  (vprint 2 "    first-blocking-asteroid ~a ~a~%" loc-1 loc-2)
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
	((and (= row row-1) (= col col-1))
	 (vprint 2 "      Clear!~%")
	 (if (char= (elt (elt *map* row) col) #\#)
	     (list row col)
	     nil))
      (vprint 2 "    Step ~a,~a~%" row col)
      (when (char= (elt (elt *map* row) col) #\#)
	(vprint 2 "      HIT!~%")
	(return-from first-blocking-asteroid (list row col))))))

(defun off-the-map (pos)
  (if (null pos)
      nil
      (let ((map-height (length *map*))
	    (map-width (length (elt *map* 0)))
	    (row (first pos))
	    (col (second pos)))
	(or (< row 0) (>= row map-height) (< col 0) (>= col map-width)))))

(defun dist-between (loc-1 loc-2)
  (sqrt (+ (expt (- (first loc-1) (first loc-2)) 2.0)
	   (expt (- (second loc-1) (second loc-2)) 2.0))))

(defun direction-to (from-loc to-loc)
  (let* ((from-row (first from-loc))
	 (from-col (second from-loc))
	 (to-row (first to-loc))
	 (to-col (second to-loc))
	 (dy (- from-row to-row))
	 (dx (- to-col from-col)))
    (if (= 0 dy)
	(+ (/ PI 2) (if (< dx 0) PI 0))
	(let ((a (atan dx dy)))
	  (vprint 2 "  from ~a to ~a dy ~a dx ~a a ~a~%" from-loc to-loc dy dx a)
	  (+ a (if (< a 0) (* 2 PI) 0))))))

(defun pos-in-dir-from (loc dir)
  (let ((dx (* *ray-epsilon* (cos dir)))
	(dy (* *ray-epsilon* (sin dir))))
    (vprint 3 "pos-in-dir-from ~a ~a~%" loc dir)
    (do* ((frow (- (first loc) dy) (- frow dy))
	  (fcol (+ (second loc) dx) (+ fcol dx))
	  (newpos (list (round frow) (round fcol)) (list (round frow) (round fcol))))
	 ((and (not (equal loc newpos)) (< (dist-between (list frow fcol) newpos) *proximity-epsilon*))
	  (list newpos (list frow fcol)))
      (vprint 3 " loc: ~a newpos: ~a  fpos: ~a loc==newpos? ~a~%" loc newpos (list frow fcol) (equal loc newpos)))))

(defun first-intersecting-asteroid (from-loc dir)
  (vprint 3 "first-intersecting-asteroid ~a ~a~%" from-loc dir)
  (do ((next-pos (pos-in-dir-from from-loc dir) (pos-in-dir-from (second next-pos) dir)))
      ((off-the-map (first next-pos)) nil)
    (vprint 3 "  next-pos: ~a~%" next-pos)
    (let ((row (first (first next-pos)))
	  (col (second (first next-pos))))
      (when (char= (elt (elt *map* row) col) #\#)
	(return-from first-intersecting-asteroid (list row col))))))
    
(defun next-clockwise-edge-location (loc)
  (let ((map-height (length *map*))
	(map-width (length (elt *map* 0)))
	(row (first loc))
	(col (second loc)))
    (cond ((= row 0)			; top edge
	   (if (< col (1- map-width))
	       (list 0 (1+ col))
	       (list 1 col)))
	  ((= row (1- map-height))	; bottom edge
	   (if (> col 0)
	       (list row (1- col))
	       (list (1- row) 0)))
	  ((> col 0)			; right edge
	   (if (< row (1- map-height))
	       (list (1+ row) col)
	       (list row (1- col))))
	  (t				; left edge
	   (if (> row 0)
	       (list (1- row) 0)
	       (list 0 1))))))

(defun dump-map ()
  (vprint 1 "map:~%")
  (loop for row across *map*
       do (vprint 1 "  ~a~%" row))
  (vprint 1 "~%"))

(defun visible-asteroids-sorted-by-angle (from-loc)
  ;; each entry is a *list* of ast-list-elem objects.
  (let ((ast-iter (new-asteroid-enumerator *map*))
	(ast-list (make-array '(1) :adjustable t :fill-pointer 0)))
    (do ((ast-loc (funcall ast-iter) (funcall ast-iter)))
	((null ast-loc))
      (let* ((angle (direction-to from-loc ast-loc))
	     (dist  (dist-between from-loc ast-loc))
	     (ast-elem (make-ast-list-elem
			:location ast-loc
			:distance dist
			:angle angle))
	     (entry-pos (position-if (lambda (n) (= (ast-list-elem-angle (car n)) angle)) ast-list)))
	(cond ((null entry-pos)
	       (vector-push-extend (list ast-elem) ast-list))
	      (t
	       (let ((l (elt ast-list entry-pos)))
		 (push ast-elem l)
		 (setf (elt ast-list entry-pos)
		       (sort l (lambda (a b)
				 (< (ast-list-elem-distance a)
				    (ast-list-elem-distance b))))))))))

    ;; - sort the main list by increasing angle
    (sort ast-list (lambda (a b)
		     (< (ast-list-elem-angle (car a))
			(ast-list-elem-angle (car b)))))
    (vprint 2 "sorted ast-list: ~a~%" ast-list)
    ast-list))

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

  (dump-map)

  (let ((num-asteroids 0)
	(max-count 0)
	(max-count-location nil)
	(ast-iter-outer (new-asteroid-enumerator *map*)))
    ;; consider the visible-count of each asteroid
    (do ((my-coords (funcall ast-iter-outer) (funcall ast-iter-outer)))
	((null my-coords))
      (incf num-asteroids)
      (let* ((ast-list (visible-asteroids-sorted-by-angle my-coords))
	     (my-count (length ast-list)))
	(when (> my-count max-count)
	  (vprint 2 "  NEW WINNER!~%")
	  (setf max-count my-count)
	  (setf max-count-location my-coords))))
    (format t "Max visibility count is ~a at ~a~%" max-count max-count-location)

    ;; After talking with Mike, here is the plan:
    ;; For part two, start at the asteroid with the best visibility. (or arbitrary loc)
    ;; - enumerate all other asteroids.  for each:
    ;;   - calculate the distance to it and the angle relative to due north
    ;;   - add the asteroids to a list of lists
    ;;     - each sublist has all asteroids that are at the same relative angle,
    ;;       sorted by increasing distance
    ;; - sort the main list by increasing angle
    ;; - set the "destroy" list to empty, and the "current index" to 0
    ;; - perform the rotations, adding each new asteroid to the destroy list
    ;; - At the end of each rotation:
    ;;   delete all the asteroids on the destroy-list from the "current index" to the end;
    ;;   set the "current index" to the length of the destroy list
    ;; - Keep rotating until only 1 asteroid is left
    ;; - print the 200th entry of the destroy list
    
    (vprint 1 "There are ~a total target asteroids.~%" num-asteroids)
    (let* (;(laser-loc '(3 8)) ; for t2
	   (laser-loc max-count-location)

	   ;; each entry is a *list* of ast-list-elem objects.
	   (ast-list (visible-asteroids-sorted-by-angle laser-loc))
	   (destroy-list (make-array '(1) :adjustable t :fill-pointer 0))
	   (destroy-list-index 0))

      ;; Do the "rotations".  Each rotation is a single traversal of the list,
      ;; removing the first sub-element of each main list entry, deleting entries
      ;; that become empty.
      ;; Stop when the main list is empty.
      (vprint 1 "Starting at location ~a~%" laser-loc)
      (do ((num-vaporized 0)
	   (rotation-count 0))
	  ((= 0 (- num-asteroids num-vaporized)))
	(vprint 1 "Doing rotation ~a. #remaining: ~a~%" (incf rotation-count) (- num-asteroids num-vaporized))
	;; Do one rotation
	(loop for i below (length ast-list)
	   do (let ((target-list (elt ast-list i)))
		(vprint 2 "adding ~a to the destroy-list.~%" (first target-list))
		(vector-push-extend (first target-list) destroy-list)))

	(when (= destroy-list-index (length destroy-list))
	  (format t "Entire rotation without finding any asteroids?~%")
	  (sb-ext:exit :code 1))
	(vprint 2 "==> ~a new asteroids to destroy~%" (- (length destroy-list) destroy-list-index))

	;; Remove the targets identified on this rotation from the main asteroid list.
	(loop for vaporee across (subseq destroy-list destroy-list-index)
	   do (let* ((target-pos (position-if (lambda (n)
						(equal (ast-list-elem-location (car n))
						       (ast-list-elem-location vaporee))) ast-list))
		     (target-list (elt ast-list target-pos))) 
		(incf num-vaporized)
		(format t "Vaporized asteroid ~a at ~a~%" num-vaporized (ast-list-elem-location vaporee))
		(if (= 1 (length target-list))
		    (setf ast-list (delete target-list ast-list))
		    (setf (elt ast-list target-pos) (cdr target-list)))
		(if (= 200 num-vaporized)
		    (format t "For the bet, ~a~%" (+ (* 100 (second (ast-list-elem-location vaporee)))
						     (first (ast-list-elem-location vaporee)))))))
	(setf destroy-list-index (length destroy-list)))))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
