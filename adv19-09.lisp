;;; sbcl --noinform --load adv19-09.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)

(load "queues.lisp")

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *progfile* "adv19-09.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

(defstruct program
  (number 0)
  code
  (pc 0)
  (relative-base 0)
  input					; list
  redirect-input			; flag
  redirect-output-to)			; program-number

(defparameter *progv* (make-array '(5)))

;; ===========================================

(defstruct operand
  pos
  encoded-value
  (mode 'POSITIONAL))

(defstruct insn
  pos
  opcode
  op-0					; operand
  op-1					; operand
  dest
  pc-increment)

(defun read-program (filename)
  (let ((l (with-open-file (in filename)
	     (loop for line = (read-line in nil)
		while line
		collect (split-sequence:split-sequence #\, line)))))
    (let* ((result (remove-if (lambda (e) (= 0 (length e))) (alexandria:flatten l)))
	   (v (make-array (list (length result)) :initial-contents result)))
      (map 'vector #'parse-integer v))))

(defun dump-program (tag program)
  (vprint 1 "~d code:~%" tag)
  (let ((progcode (program-code program)))
    (loop for i below (length progcode)
       do (vprint 1 "~4,,,0@d: ~a~%" i (elt progcode i)))))

(defun dump-prog-state (tag pgm)
  (vprint 1 "Prog ~a: pc ~a input ~a r-input ~a r-output ~a~%" tag
	  (program-pc pgm)
	  (if (program-input pgm) (dls:members (program-input pgm)) nil)
	  (program-redirect-input pgm)
	  (program-redirect-output-to pgm)))

;; ===========================================
	   
(defun input-number (program)
  (let ((r (program-input program)))
    (vprint 1 "input from prog ~a: avail ~a~%" (program-number program) (if r (dls:members r) nil))
    (let ((result (cond ((null r)
			 (vprint 1 "Reading number from stdin...~%")
			 (parse-integer (read-line *STANDARD-INPUT* nil)))
			(t
			 (vprint 1 "input redirected~%")
			 (cond ((dls:empty r)
				(when (= *part* 1)
				  (format t "EOF on input program ~a~%" (program-number program))
				  (sb-ext:exit :code 1))
				nil)
			       (t
				(dls:dequeue r)))))))
      (vprint 1 "Input result: ~a~%" result)
      result)))

;; Output is redirected base on the value of the redirect-output member of the program object:
;;   nil       -- no redirection
;;   number    -- output goes to input queue of program with the given number
;;   otherwise -- redirection value is assumed to be a queue object, and output goes there.
(defun output-number (program value)
  (let ((redirection (program-redirect-output-to program)))
    (vprint 1 "prog[~a] output ~a (redirection ~a)~%" (program-number program) value redirection)
    (cond ((null redirection)		; not redirected
	   (format t "OUTPUT: ~a~%" value))
	  ((numberp redirection)	; redirected to another numbered program
	   (vprint 1 "  redirected to program ~a~%" redirection)
	   (dls:enqueue (program-input (elt *progv* redirection)) value))
	  (t				; redirected to a list
	   (vprint 1 "  redirected to queue ~a~%" redirection)
	   (dls:enqueue redirection value)))))

(defmacro op-mode (str pos)
  `(cond ((char= (char ,str ,pos) #\1)
	  'IMMEDIATE)
	 ((char= (char ,str ,pos) #\2)
	  'RELATIVE)
	 (t
	  'POSITIONAL)))

(defun mem-read (program pos)
  (when (< pos 0)
    (format t "reading pos ~a is invalid~%" pos)
    (sb-ext:exit :code 1))
  (let ((code (program-code program)))
    (when (>= pos (length code))
      (vprint 3 "Expanding program from length ~a to length ~a~%" (length code) (1+ pos))
      (dotimes (i (1+ (- pos (length code))))
	(vector-push-extend 0 code)))
    (vprint 3 "Reading: mem[~a] = ~a~%" pos (elt code pos))
    (elt code pos)))

(defun mem-write (program pos value)
  (when (< pos 0)
    (format t "writing pos ~a is invalid~%" pos)
    (sb-ext:exit :code 1))
  (let ((code (program-code program)))
    (when (>= pos (length code))
      (vprint 3 "Expanding program from length ~a to length ~a~%" (length code) (1+ pos))
      (dotimes (i (1+ (- pos (length code))))
	(vector-push-extend 0 code)))
    (vprint 3 "Writing: mem[~a] ~a -> ~a~%" pos (elt code pos) value)
    (setf (elt code pos) value)))

(defun op-addr (program operand)
  (if (eql (operand-mode operand) 'IMMEDIATE)
      (operand-encoded-value operand)
      (+ (operand-encoded-value operand)
	 (if (eql (operand-mode operand) 'POSITIONAL)
	     0
	     (program-relative-base program)))))

(defun op-val (program operand)
  (if (eql (operand-mode operand) 'IMMEDIATE)
      (operand-encoded-value operand)
      (mem-read program
		 (+ (operand-encoded-value operand)
		    (if (eql (operand-mode operand) 'POSITIONAL)
			0
			(program-relative-base program))))))

;; Decodes to: (opcode op-0-mode op-1-mode dest-pos pc-increment)
(defun decode-insn (program)
  (let* ((pc (program-pc program))
	 (op-str (format nil "~5@a" (mem-read program pc)))
	 (tmpstr (reverse op-str))
	 (opcode (parse-integer (nreverse (subseq (copy-seq tmpstr) 0 2))))
	 (op-0-mode (op-mode tmpstr 2))
	 (op-1-mode (op-mode tmpstr 3))
	 (dest-mode (op-mode tmpstr 4))
	 (tmsg (format nil " -- opcode ~a modes: ~a,~a~%" opcode op-0-mode op-1-mode)))
    (cond ((= opcode 99)		; halt
	   (return-from decode-insn (make-insn :pos pc :opcode 99 :pc-increment 0)))

	  ((or (= opcode 1) (= opcode 2) (= opcode 7) (= opcode 8)) ; +/*/less-than/equals
	   (let ((op-0-encoded-val (mem-read program (+ pc 1)))
		 (op-1-encoded-val (mem-read program (+ pc 2)))
		 (dest-pos (mem-read program (+ pc 3))))
	     (vprint 2 "  DECODE[~a]: ~a ~a ~a ~a ~a" pc op-str op-0-encoded-val op-1-encoded-val dest-pos tmsg)
	     (return-from decode-insn
	       (make-insn
		:opcode opcode
		:pos pc
		:op-0 (make-operand :pos (+ pc 1) :mode op-0-mode :encoded-value op-0-encoded-val)
		:op-1 (make-operand :pos (+ pc 2) :mode op-1-mode :encoded-value op-1-encoded-val)
		:dest (make-operand :pos (+ pc 3) :mode dest-mode :encoded-value dest-pos)
		:pc-increment 4))))

	  ((or (= opcode 3) (= opcode 4))		; input/output
	   (let ((op-0-encoded-val (mem-read program (+ pc 1))))
	     (vprint 2 "  DECODE[~a]: ~a ~a ~a" pc op-str op-0-encoded-val tmsg)
	     (return-from decode-insn
	       (make-insn
		:opcode opcode
		:pos pc
		:op-0 (make-operand :pos (+ pc 1) :mode op-0-mode :encoded-value op-0-encoded-val)
		:pc-increment 2))))

	  ((or (= opcode 5) (= opcode 6)) ; jump-if-true/jump-if-false
	   (let ((op-0-encoded-val (mem-read program (+ pc 1)))
		 (dest-pos (mem-read program (+ pc 2))))
	     (vprint 2 "  DECODE[~a]: ~a ~a ~a ~a" pc op-str op-0-encoded-val dest-pos tmsg)
	     (return-from decode-insn
	       (make-insn
		:opcode opcode
		:pos pc
		:op-0 (make-operand :pos (+ pc 1) :mode op-0-mode :encoded-value op-0-encoded-val)
		:op-1 (make-operand :pos (+ pc 2) :mode op-1-mode :encoded-value dest-pos)
		:pc-increment 3))))

	  ((= opcode 9)			; adjust relative base
	   (let ((op-0-encoded-val (mem-read program (+ pc 1))))
	     (vprint 2 "  DECODE[~a]: ~a ~a ~a" pc op-str op-0-encoded-val tmsg)
	     (return-from decode-insn
	       (make-insn
		:opcode opcode
		:pos pc
		:op-0 (make-operand :pos (+ pc 1) :mode op-0-mode :encoded-value op-0-encoded-val)
		:pc-increment 2))))

	   (t
	    (format t "Unsupported opcode[~a]: ~a~%" pc opcode)
	    (sb-ext:exit :code 1)))))

;; returns:
;;  'SLICE if input runs dry (if = 2 *part*), or output is produced (if = 2 *part*)
;;  'HALT if halted
(defun run-intcode-program (program)
  (vprint 1 "Running program ~a~%" (program-number program))
  (do ((quit nil))
      (quit quit)
    (let ((insn (decode-insn program)))
      (vprint 2 "    raw insn: ~a~%" insn)
      (cond ((= 1 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn)))
		   (dest-pos (op-addr program (insn-dest insn))))
	       (vprint 2 "  TRACE[~a][~a]: mem[~a] = ~a + ~a = ~a~%" (program-number program) (program-pc program)
		       dest-pos operand-0 operand-1 (+ operand-0 operand-1))
	       (mem-write program dest-pos (+ operand-0 operand-1))
	       (incf (program-pc program) (insn-pc-increment insn))))

	    ((= 2 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn)))
		   (dest-pos (op-addr program (insn-dest insn))))
	       (vprint 2 "  TRACE[~a][~a]: : mem[~a] = ~a * ~a = ~a~%" (program-number program) (program-pc program)
		       dest-pos operand-0 operand-1 (* operand-0 operand-1))
	       (mem-write program dest-pos (* operand-0 operand-1))
	       (incf (program-pc program) (insn-pc-increment insn))))

	    ((= 3 (insn-opcode insn))
	     (let ((dest-pos (op-addr program (insn-op-0 insn)))
		   (input-val (input-number program)))
	       (vprint 2 "  TRACE[~a][~a]: INPUT mem[~a] = ~a~%"
		       (program-number program) (program-pc program) dest-pos input-val)
	       (cond ((null input-val)
		      (setf quit 'SLICE))
		     (t
		      (mem-write program dest-pos input-val)))
	       (incf (program-pc program) (insn-pc-increment insn))))

	    ((= 4 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn))))
	       (output-number program operand-0)
	       (when (= *part* 2)
		 (setf quit 'SLICE))
	       (incf (program-pc program) (insn-pc-increment insn))))
	    
	    ((= 5 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn))))
	       (if (/= 0 operand-0)
		   (vprint 2 "  TRACE[~a][~a]: JNE0 ~a /= 0 -- jumping to position ~a~%" (program-number program) (program-pc program)
			   operand-0 operand-1)
		   (vprint 2 "  TRACE[~a][~a]: JNE0 ~a = 0 -- not jumping~%" (program-number program) (program-pc program)
			   operand-0))
	       (if (/= 0 operand-0)
		   (setf (program-pc program) operand-1)
		   (incf (program-pc program) (insn-pc-increment insn)))))
	    
	    ((= 6 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn))))
	       (if (= 0 operand-0)
		   (vprint 2 "  TRACE[~a][~a]: JEQ0 ~a = 0 -- jumping to position ~a~%" (program-number program) (program-pc program)
			   operand-0 operand-1)
		   (vprint 2 "  TRACE[~a][~a]: JEQ0 ~a /= 0 -- not jumping~%" (program-number program) (program-pc program)
			   operand-0))
	       (if (= 0 operand-0)
		   (setf (program-pc program) operand-1)
		   (incf (program-pc program) (insn-pc-increment insn)))))

	    ((= 7 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn)))
		   (dest-pos (op-addr program (insn-dest insn))))
	       (vprint 2 "  TRACE[~a][~a]: SETLT mem[~a] = ~a < ~a~%" (program-number program) (program-pc program)
		       dest-pos operand-0 operand-1)
	       (mem-write program dest-pos (if (< operand-0 operand-1) 1 0))
	       (incf (program-pc program) (insn-pc-increment insn))))

	    ((= 8 (insn-opcode insn))
	     (let ((operand-0 (op-val program (insn-op-0 insn)))
		   (operand-1 (op-val program (insn-op-1 insn)))
		   (dest-pos (op-addr program (insn-dest insn))))
	       (vprint 2 "  TRACE[~a][~a]: SETEQ mem[~a] = ~a == ~a~%" (program-number program) (program-pc program)
		       dest-pos operand-0 operand-1)
	       (mem-write program dest-pos (if (= operand-0 operand-1) 1 0))
	       (incf (program-pc program) (insn-pc-increment insn))))

	    ((= 9 (insn-opcode insn))	; adjust relative base
	     (vprint 2 "  TRACE[~a][~a]: adjust relative base ~a += ~a~%" (program-number program) (program-pc program)
		     (program-relative-base program) (op-val program (insn-op-0 insn)))
	     (incf (program-relative-base program) (op-val program (insn-op-0 insn)))
	     (incf (program-pc program) (insn-pc-increment insn)))
	    
	    ((= 99 (insn-opcode insn))
	     (vprint 2 "  TRACE[~a][~a]: HALT~%" (program-number program) (program-pc program))
	     (setf quit 'HALT))

	    (t
	     (format t "Unexpected opcode ~a at pc ~a~%" (mem-read program (program-pc program)) (program-pc program))
	     (return-from run-intcode-program 1))))))

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
		 (setf *progfile* (cdr arg)))))
	(setf args new-args)))
  
  (let ((code (read-program *progfile*))
	(program (make-program)))
    (setf (program-code program)
	  (make-array (list (length code)) :fill-pointer (length code) :adjustable t :initial-contents code))
    (setf (elt *progv* 0) program))

    
  (dump-program "Original" (elt *progv* 0))
	       
  (when (> *verbose* 1)
    (dump-prog-state "orig" (elt *progv* 0)))
      
  (do ((done nil))
      (done)
    (let ((result (run-intcode-program (elt *progv* 0))))
      (vprint 1 "Result[~a] is ~a~%" 0 result)
      (if (eql result 'HALT)
	  (setf done t))))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
