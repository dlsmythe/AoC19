;;; sbcl --noinform --load adv19-05.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)

(defparameter *verbose* nil)
(defparameter *part* 1)
(defparameter *progfile* "adv19-05.input")
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
    (let* ((result (remove-if (lambda (e) (= 0 (length e))) (alexandria:flatten l)))
	   (v (make-array (list (length result)) :initial-contents result)))
      (map 'vector #'parse-integer v))))

(defun dump-program (tag prog)
  (vprint 1 "~a prog:~%" tag)
  (loop for i below (length prog)
       do (vprint 1 "~4,,,0@d: ~a~%" i (elt prog i))))

(defun input-number ()
  (let ((str (read-line *STANDARD-INPUT* nil)))
    (parse-integer str)))

(defun decode-opcode (pc)
  (let* ((op-str (format nil "~4@a" (elt *prog* pc)))
	 (tmpstr (reverse op-str))
	 (opcode (parse-integer (nreverse (subseq (copy-seq tmpstr) 0 2))))
	 (op-0-immediate (char= (char tmpstr 2) #\1))
	 (op-1-immediate (char= (char tmpstr 3) #\1))
	 (tmsg (format nil " -- opcode ~a imm: ~a,~a" opcode op-0-immediate op-1-immediate)))
    (cond ((= opcode 99)
	   (return-from decode-opcode (values opcode nil nil nil 1)))

	  ((or (= opcode 1) (= opcode 2) (= opcode 7) (= opcode 8)) ; +/*/less-than/equals
	   (let ((op-0-val (elt *prog* (+ pc 1)))
		 (op-1-val (elt *prog* (+ pc 2)))
		 (dest-pos (elt *prog* (+ pc 3))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a ~a ~a" pc op-str op-0-val op-1-val dest-pos tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt *prog* op-0-val))
		(if op-1-immediate op-1-val (elt *prog* op-1-val))
		dest-pos
		4))))

	  ((= opcode 3)			; input
	   (let ((op-0-val (elt *prog* (+ pc 1))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a" pc op-str op-0-val tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(input-number)
		nil
		op-0-val
		2))))

	  ((= opcode 4)			; output
	   (let ((op-0-val (elt *prog* (+ pc 1))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a" pc op-str op-0-val tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt *prog* op-0-val))
		nil
		nil
		2))))

	  ((or (= opcode 5) (= opcode 6)) ; jump-if-true/jump-if-false
	   (let ((op-0-val (elt *prog* (+ pc 1)))
		 (dest-pos (elt *prog* (+ pc 2))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a ~a" pc op-str op-0-val dest-pos tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt *prog* op-0-val))
		nil
		(if op-1-immediate dest-pos (elt *prog* dest-pos))
		3))))

	   (t
	    (format t "Unsupported opcode: ~a~%" opcode)
	    (sb-ext:exit :code 1)))))

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
		 (setf *default-progfile* nil)
		 (setf *progfile* (cdr arg)))))
	(setf args new-args)))

  (setf *prog* (read-program *progfile*))
  (dump-program "INITIAL" *prog*)

  (do ((pc 0)
       (quit nil))
      (quit)
    (multiple-value-bind (opcode operand-0 operand-1 result-position pc-increment) (decode-opcode pc)
      (vprint 2 " op0: ~a op1: ~a destaddr: ~a pc-incr: ~a~%"
	      operand-0 operand-1 result-position pc-increment)
	
      (cond ((= 1 opcode)
	     (vprint 2 "TRACE: mem[~a] = ~a + ~a = ~a~%" result-position operand-0 operand-1 (+ operand-0 operand-1))
	     (setf (elt *prog* result-position) (+ operand-0 operand-1))
	     (incf pc pc-increment))

	    ((= 2 opcode)
	     (vprint 2 "TRACE: mem[~a] = ~a * ~a = ~a~%" result-position operand-0 operand-1 (+ operand-0 operand-1))
	     (setf (elt *prog* result-position) (* operand-0 operand-1))
	     (incf pc pc-increment))

	    ((= 3 opcode)
	     (vprint 2 "TRACE: INPUT mem[~a] = ~a~%" result-position operand-0)
	     (setf (elt *prog* result-position) operand-0)
	     (incf pc pc-increment))

	    ((= 4 opcode)
	     (format t "OUTPUT: ~a~%" operand-0)
	     (incf pc pc-increment))
	    
	    ((= 5 opcode)
	     (if (/= 0 operand-0)
		 (vprint 2 "TRACE: JNE0 ~a /= 0 -- jumping to position ~a~%" operand-0 result-position)
		 (vprint 2 "TRACE: JNE0 ~a = 0 -- not jumping~%" operand-0))
	     (if (/= 0 operand-0)
		 (setf pc result-position)
		 (incf pc pc-increment)))
	    
	    ((= 6 opcode)
	     (if (= 0 operand-0)
		 (vprint 2 "TRACE: JEQ0 ~a = 0 -- jumping to position ~a~%" operand-0 result-position)
		 (vprint 2 "TRACE: JEQ0 ~a /= 0 -- not jumping~%" operand-0))
	     (if (= 0 operand-0)
		 (setf pc result-position)
		 (incf pc pc-increment)))

	    ((= 7 opcode)
	     (vprint 2 "TRACE: SETLT mem[~a] = ~a < ~a~%" result-position operand-0 operand-1)
	     (setf (elt *prog* result-position) (if (< operand-0 operand-1) 1 0))
	     (incf pc pc-increment))

	    ((= 8 opcode)
	     (vprint 2 "TRACE: SETEQ mem[~a] = ~a == ~a~%" result-position operand-0 operand-1)
	     (setf (elt *prog* result-position) (if (= operand-0 operand-1) 1 0))
	     (incf pc pc-increment))
	     
	    ((= 99 opcode)
	     (setf quit t))

	    (t
	     (format t "Unexpected opcode ~a at pc ~a~%" (elt *prog* pc) pc)
	     (return-from main 1)))))

  (dump-program "FINAL" *prog*)

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
