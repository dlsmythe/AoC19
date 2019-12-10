;;; sbcl --noinform --load adv19-06.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defparameter *verbose* nil)
(defparameter *part* 1)
(defparameter *input-file* "adv19-06.input")

(defparameter *orbits* (make-hash-table :test 'equal))

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defstruct orbitee (count 0) (orbiting nil) (name nil))

(defun add-orbit (orbitee-name orbiter-name)
  (let ((orbitee (gethash orbitee-name *orbits*))
	(orbiter (gethash orbiter-name *orbits*)))
    (when (null orbitee)
      (vprint 2 "Created orbitee ~a~%" orbitee-name)
      (setf orbitee (make-orbitee :name orbitee-name))
      (setf (gethash orbitee-name *orbits*) orbitee))
    (when (null orbiter)
      (vprint 2 "Created orbiter ~a~%" orbiter-name)
      (setf orbiter (make-orbitee :name orbiter-name :orbiting orbitee))
      (setf (gethash orbiter-name *orbits*) orbiter))
    (setf (orbitee-orbiting orbiter) orbitee)
    (vprint 2 "Add orbit ~a orbiting ~a~%" orbiter-name orbitee-name)))

(defun read-orbits (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
       while line
       do (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^(.+)[)](.+)" line)
	    (when (not parsedok)
	      (format t "bad input ~%")
	      (sb-ext:exit :code 1))
	    (add-orbit (elt vals 0) (elt vals 1))))))

(defun orbit-count (orbiter)
  (let ((orbiting (orbitee-orbiting orbiter)))
    (if orbiting
	(+ 1 (orbit-count orbiting))
	0)))

(defun get-orbits (orbit-name)
  (vprint 1 "get-orbits ~a~%" orbit-name)
  (let ((base-orb (gethash orbit-name *orbits*)))
    (labels ((get-orbits-work (orb)
	       (let ((name (orbitee-name orb))
		     (orbiting (orbitee-orbiting orb)))
		 (vprint 1 "get-orbits-work ~a: orbiting ~a~%" name (if (null orbiting) "nil" (orbitee-name orbiting)))
		 (if (null orbiting)
		   nil
		   (cons name (get-orbits-work orbiting))))))
      (nreverse (get-orbits-work base-orb)))))

(defun first-common-orbit ()
  (let ((my-orbits (get-orbits "YOU"))
	(santa-orbits (get-orbits "SAN")))
    (vprint 1 "my-orbits: ~a~%" my-orbits)
    (vprint 1 "santa-orbits: ~a~%" santa-orbits)
    (do ((prev nil)
	 (l-me my-orbits (cdr l-me))
	 (l-san santa-orbits (cdr l-san)))
	((string/= (car l-me) (car l-san)) prev)
      (vprint 1 "prev ~a~%" (car l-me))
      (setf prev (car l-me)))))

;; args are names (strings)
(defun orbits-to (dest src)
  (vprint 1 "orbits-to ~a ~a~%" dest src)
  (if (string= dest src)
      0
      (+ 1 (orbits-to dest (orbitee-name (orbitee-orbiting (gethash src *orbits*)))))))

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

  (read-orbits *input-file*)
  (format t "There are ~a elements in *orbits*~%" (hash-table-count *orbits*))

  ;; Calculate the orbit counts
  (maphash (lambda (k v)
	     (declare (ignore k))
	     (setf (orbitee-count v) (orbit-count v)))
	   *orbits*)
  
  ;; for part 1, add all orbitees' orbit-counts
  (format t "Total: ~a~%" (loop for orbitee being the hash-values in *orbits*
			     sum (orbitee-count orbitee)))

  (let ((common-orbit-name (first-common-orbit))
	(san (gethash "SAN" *orbits*))
	(me (gethash "YOU" *orbits*)))
    (vprint 1 "first common ~a~%" common-orbit-name)
    (format t "need ~a transfers~%"
	    (+ (orbits-to common-orbit-name (orbitee-name (orbitee-orbiting san)))
	       (orbits-to common-orbit-name (orbitee-name (orbitee-orbiting me))))))
  
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
