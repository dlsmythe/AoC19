;;; sbcl --noinform --load adv19-02.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *verbose* nil)
(defparameter *trace* nil)
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
		("t" :none nil)
		("f" :required nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "t" (car arg))
		 (setf *trace* t))
		((string= "f" (car arg))
		 (setf *default-progfile* nil)
		 (setf *progfile* (cdr arg)))))
	(setf args new-args)))

  (setf *prog* (read-program *progfile*))
  (vprint 1 "*prog* is ~A~%" *prog*)

  ;; Once you have a working computer, the first step is to restore the
  ;; gravity assist program (your puzzle input) to the "1202 program alarm" 
  ;; state it had just before the last computer caught fire. To do this,
  ;; before running the program, replace position 1 with the value 12
  ;; and replace position 2 with the value 2.
  ;;  What value is left at position 0 after the program halts?
  (when *default-progfile*
    (setf (elt *prog* 1) 12)
    (setf (elt *prog* 2) 2))

  (do ((pc 0))
      ((= 99 (elt *prog* pc)))
    (vprint 1 "prog[~a]: ~a~%" pc (elt *prog* pc))
    (cond ((= (elt *prog* pc) 1)
	   (let* ((dest-addr (elt *prog* (+ pc 3)))
		  (operand-0-addr  (elt *prog* (+ pc 1)))
		  (operand-1-addr  (elt *prog* (+ pc 2)))
		  (operand-0 (elt *prog* operand-0-addr))
		  (operand-1 (elt *prog* operand-1-addr)))
	     (vprint 1 "  mem[~a] = mem[~a] (~a) + mem[~a] (~a) = ~a~%"
		     dest-addr operand-0-addr operand-0 operand-1-addr operand-1 (+ operand-0 operand-1))
	     (setf (elt *prog* dest-addr) (+ operand-0 operand-1))
	     (incf pc 4)))
	  ((= (elt *prog* pc) 2)
	   (let* ((dest-addr (elt *prog* (+ pc 3)))
		  (operand-0-addr  (elt *prog* (+ pc 1)))
		  (operand-1-addr  (elt *prog* (+ pc 2)))
		  (operand-0 (elt *prog* operand-0-addr))
		  (operand-1 (elt *prog* operand-1-addr)))
	     (vprint 1 "  mem[~a] = mem[~a] (~a) * mem[~a] (~a) = ~a~%"
		     dest-addr operand-0-addr operand-0 operand-1-addr operand-1 (* operand-0 operand-1))
	     (setf (elt *prog* dest-addr) (* operand-0 operand-1))
	     (incf pc 4)))
	  (t
	   (format t "Unexpected opcode ~a at pc ~a~%" (elt *prog* pc) pc)
	   (return-from main 1))))

  (format t "Pos 0 = ~A~%" (elt *prog* 0))
  (vprint 2 "final *prog* is ~A~%" *prog*)
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
