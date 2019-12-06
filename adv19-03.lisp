;;; sbcl --noinform --load adv19-03.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *verbose* nil)
(defparameter *trace* nil)
(defparameter *part* 1)
(defparameter *input* nil)
(defparameter *input-file* "adv19-03.input")

(defparameter *position-hash* (make-hash-table :test 'equal))

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-input (filename)
  (let ((l (with-open-file (in filename)
	     (loop for line = (read-line in nil)
		while line
		collect (split-sequence:split-sequence #\, line)))))
    (when (> (length l) 2)
      (format t "Too many lines in input")
      (sb-ext:exit :code 1))
    l))

(defun add-pos (path-num x y steps)
  (vprint 1 "add-pos path ~a ~a/~a steps ~a~%" path-num x y steps)
  (let ((hashkey (format nil "~a/~a" x y)))
    ;; visitlog format: (MH (visit-count-0 visit-count-1) (x y) (first-dist-0 first-dist-1)
    (let ((visitlog (gethash hashkey *position-hash*))
	  (mh (+ (abs x) (abs y))))
      (cond ((null visitlog)
	     (vprint 2 "Adding position ~a/~a path ~a mh ~a steps ~a~%" x y path-num mh steps)
	     (setf (gethash hashkey *position-hash*)
		   (list mh
			 (if (= 0 path-num) (list 1 0) (list 0 1))
			 (list x y)
			 (if (= 0 path-num) (list steps 0) (list 0 steps)))))
	    (t
	     (vprint 2 "Updating ~a position ~a/~a  path ~a mh ~a steps ~a~%" visitlog x y path-num mh (elt (fourth visitlog) path-num))
	     (setf (elt (fourth visitlog) path-num) steps)
	     (incf (elt (second visitlog) path-num)))))
    (vprint 2 "pos ~a is now ~a~%" hashkey (gethash hashkey *position-hash*))))
  
(defun traverse-path (path-num)
  "Follow path, updating *position-hash*.  Return max manhattan-distance of the path."
  
  (let ((path (elt *input* path-num))
	(max-mh 0)
	(x 0)
	(y 0)
	(steps 0))
    (loop for pos in path
       do (let ((dir (char pos 0))
		(dist (parse-integer (subseq pos 1))))
	    (cond ((char= dir #\U)
		   (loop for yy from (1+ y) upto (+ y dist)
			do (add-pos path-num x yy (incf steps)))
		   (incf y dist))
		  ((char= dir #\D)
		   (loop for yy from (1- y) downto (- y dist)
			do (add-pos path-num x yy (incf steps)))
		   (decf y dist))
		  ((char= dir #\L)
		   (loop for xx from (1- x) downto (- x dist)
			do (add-pos path-num xx y (incf steps)))
		   (decf x dist))
		  ((char= dir #\R)
		   (loop for xx from (1+ x) upto (+ x dist)
			do (add-pos path-num xx y (incf steps)))
		   (incf x dist)))
	    (let ((mh (+ (abs x) (abs y))))
	      (vprint 1 "path ~a pos ~a: (~a/~a) dir ~a dist ~a mh ~a~%" path-num pos x y dir dist mh)
	      (when (> mh max-mh)
		(setf max-mh mh)))))
    (vprint 1 "Max-mh for path ~a is ~a~%" path-num max-mh)
    max-mh))

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("v" :required nil)
		("p" :required nil)
		("t" :none nil)
		("f" :required nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "p" (car arg))
		 (setf *part* (parse-integer (cdr arg))))
		((string= "t" (car arg))
		 (setf *trace* t))
		((string= "f" (car arg))
		 (setf *input-file* (cdr arg)))))
	(setf args new-args)))

  (setf *input* (read-input *input-file*))
  (vprint 1 "*input* is ~A~%" *input*)

  ;; loop across both wire paths, remembering each position
  ;;   at each step
  ;;     remember current position (keep per-wire counter at each spot)
  ;;     calculate manhattan distance current position and update max-seen-so-far
  ;;     hash key is "X/Y"
  ;;     value remembered is: (MH (visit-count-0 visit-count-1) (x y))
  ;; reviewing the remembered positions:
  ;;   set min-mh to max-seen-so-far+1
  ;;   for each position with both visit-counts > 0:
  ;;     calculate 
  ;;     if manhattan distance < min-mh:
  ;;       update min-mh
  ;;       remember this position
  ;; print min-mh and its position
  (let ((max-mh 0))
    (let ((mh (traverse-path 0)))
      (when (> mh max-mh)
	(setf max-mh mh)))
    (let ((mh (traverse-path 1)))
      (when (> mh max-mh)
	(setf max-mh mh)))
    (vprint 2 "Position hash is:")

    (loop for pos-str being the hash-keys in *position-hash* using (hash-value visitlog)
	 do (vprint 2 "Hash-key: ~a  Value: ~a~%" pos-str visitlog))

    (let ((min-mh (1+ max-mh))
	  (min-delay MOST-POSITIVE-FIXNUM)
	  (x (1+ max-mh))
	  (y (1+ max-mh)))
      (loop for pos-str being the hash-keys in *position-hash* using (hash-value visitlog)
	 do (let ((mh (first visitlog))
		  (delay (+ (first (fourth visitlog)) (second (fourth visitlog))))
		  (visits-0 (first (second visitlog)))
		  (visits-1 (second (second visitlog))))
	      (vprint 2 "Examining ~a: ~a v0: ~a  v1: ~a~%" pos-str visitlog visits-0 visits-1)
	      (when (and (> visits-0 0) (> visits-1 0))
		(vprint 1 "Intersection at ~a: ~a~%" pos-str visitlog)
		(let ((pos (third visitlog)))
		  (if (= *part* 1)
		      (when (< mh min-mh)
			(setf min-mh mh)
			(setf x (first pos))
			(setf y (second pos))))
		    (when (< delay min-delay)
			(setf min-delay delay)
			(setf x (first pos))
			(setf y (second pos)))))))
	   
      (if (= *part* 1)
	  (if (= (1+ max-mh) min-mh)
	      (format t "paths never cross~%")
	      (format t "Min MH is ~a at position ~a, ~a~%" min-mh x y))
	  (format t "Min delay is ~a at position ~a, ~a~%" min-delay x y))))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
