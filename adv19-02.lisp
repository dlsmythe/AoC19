;;; sbcl --noinform --load adv19-02.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *verbose* nil)
(defparameter *trace* nil)
(defparameter *part* 1)
(defparameter *progfile* "adv19-02.input")
(defparameter *default-progfile* t)
(defparameter *prog* nil)

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defun read-program (filename)
  (let ((l (with-open-file (in filename)
	     (loop for line = (read-line in nil)
		while line
		collect (split-sequence:split-sequence #\, line)))))
    (let ((v (make-array (list (length (car l))) :initial-contents (car l))))
      (map 'vector #'parse-integer v))))

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
		 (setf *default-progfile* nil)
		 (setf *progfile* (cdr arg)))))
	(setf args new-args)))

  (setf *prog* (read-program *progfile*))
  (vprint 1 "*prog* is ~A~%" *prog*)

  (block outer-loop
    (do ((noun 0 (1+ noun)))
	((= 100 noun))
      (do ((verb 0 (1+ verb)))
	  ((= 100 verb))
	(let ((program (copy-seq *prog*)))

	  (vprint 1 "Noun ~a Verb ~a~%" noun verb)
	  
	  ;; Once you have a working computer, the first step is to restore the
	  ;; gravity assist program (your puzzle input) to the "1202 program alarm" 
	  ;; state it had just before the last computer caught fire. To do this,
	  ;; before running the program, replace position 1 with the value 12
	  ;; and replace position 2 with the value 2.
	  ;;  What value is left at position 0 after the program halts?
	  (if (= *part* 1)
	      (when *default-progfile* 
		(format t "Setting noun/verb to defaults for part 1~%")
		(setf (elt program 1) 12)
		(setf (elt program 2) 2))
	      (progn (setf (elt program 1) noun)
		     (setf (elt program 2) verb)))

	  (do ((pc 0))
	      ((= 99 (elt program pc)))
	    (vprint 2 "prog[~a]: ~a~%" pc (elt program pc))
	    (cond ((= (elt program pc) 1)
		   (let* ((dest-addr (elt program (+ pc 3)))
			  (operand-0-addr  (elt program (+ pc 1)))
			  (operand-1-addr  (elt program (+ pc 2)))
			  (operand-0 (elt program operand-0-addr))
			  (operand-1 (elt program operand-1-addr)))
		     (vprint 2 "  mem[~a] = mem[~a] (~a) + mem[~a] (~a) = ~a~%"
			     dest-addr operand-0-addr operand-0 operand-1-addr operand-1 (+ operand-0 operand-1))
		     (setf (elt program dest-addr) (+ operand-0 operand-1))
		     (incf pc 4)))
		  ((= (elt program pc) 2)
		   (let* ((dest-addr (elt program (+ pc 3)))
			  (operand-0-addr  (elt program (+ pc 1)))
			  (operand-1-addr  (elt program (+ pc 2)))
			  (operand-0 (elt program operand-0-addr))
			  (operand-1 (elt program operand-1-addr)))
		     (vprint 2 "  mem[~a] = mem[~a] (~a) * mem[~a] (~a) = ~a~%"
			     dest-addr operand-0-addr operand-0 operand-1-addr operand-1 (* operand-0 operand-1))
		     (setf (elt program dest-addr) (* operand-0 operand-1))
		     (incf pc 4)))
		  (t
		   (format t "Unexpected opcode ~a at pc ~a~%" (elt program pc) pc)
		   (return-from main 1))))

	  (when (= *part* 1)
	    (setf *prog* program)
	    (return-from outer-loop))
	  
	  (when (= 19690720 (elt program 0))
	    (format t "Required noun/verb: ~a/~a~%" noun verb)
	    (return-from main 0))))))

  (if (= *part* 1)
      (format t "Pos 0 = ~A~%" (elt *prog* 0))
      (format t "Required verb/noun not found.~%"))
  (vprint 2 "final *prog* is ~A~%" *prog*)

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
