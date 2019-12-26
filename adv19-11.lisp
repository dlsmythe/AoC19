;;; sbcl --noinform --load adv19-11.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "cl-ppcre" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)

(load "queues.lisp")

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *progfile* "adv19-11.input")

(defmacro vprint (level &rest body)
  `(when (>= *verbose* ,level)
     (format t ,@body)))

;; ===========================================

(defstruct program
  (number 0)
  code
  (pc 0)
  (relative-base 0)
  input					; list, queue, function...
  redirect-output-to)			; program-number

(defparameter *progv* (make-array '(5)))

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
  (vprint 1 "Prog ~a: pc ~a input ~a r-output ~a~%" tag
	  (program-pc pgm)
	  (if (program-input pgm) (dls:members (program-input pgm)) nil)
	  (program-redirect-output-to pgm)))

;; ===========================================
	   
(defun input-number (program)
  (let ((r (program-input program)))
;;    (vprint 1 "input from prog ~a: avail ~a~%" (program-number program) (if r (dls:members r) nil))
    (vprint 2 "TYPE-OF ~a is ~a~%" r (type-of r))
    (let ((result (cond ((null r)
			 (vprint 2 "Reading number from stdin...~%")
			 (parse-integer (read-line *STANDARD-INPUT* nil)))
			((eql (type-of r) 'FUNCTION)
			 (vprint 2 "Calling input hook...~%")
			 (funcall r))
			(t
			 (vprint 2 "input coming from queue~%")
			 (cond ((dls:empty r)
				(when (= *part* 1)
				  (format t "EOF on input program ~a~%" (program-number program))
				  (sb-ext:exit :code 1))
				nil)
			       (t
				(dls:dequeue r)))))))
      (vprint 2 "Input result: ~a~%" result)
      result)))

;; Output is redirected base on the value of the redirect-output member of the program object:
;;   nil       -- no redirection
;;   number    -- output goes to input queue of program with the given number
;;   otherwise -- redirection value is assumed to be a queue object, and output goes there.
(defun output-number (program value)
  (let ((redirection (program-redirect-output-to program)))
    (vprint 2 "prog[~a] output ~a (redirection ~a)~%" (program-number program) value redirection)
    (cond ((null redirection)		; not redirected
	   (format t "OUTPUT: ~a~%" value))
	  ((eql (type-of redirection) 'FUNCTION)
	   (funcall redirection value))
	  ((numberp redirection)	; redirected to another numbered program
	   (vprint 2 "  redirected to program ~a~%" redirection)
	   (dls:enqueue (program-input (elt *progv* redirection)) value))
	  (t				; redirected to a list
	   (vprint 2 "  redirected to queue ~a~%" redirection)
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

;; ===========================================

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

  ;(dump-program "Original" (elt *progv* 0))

  (let ((panels-seen (make-hash-table :test #'equal))
	(total-steps 0)
	(x 0)
	(y 0)
	(min-x most-positive-fixnum)
	(max-x most-negative-fixnum)
	(min-y most-positive-fixnum)
	(max-y most-negative-fixnum)
	(dir 'NORTH))
    
    (setf (gethash (format nil "~a ~a" x y) panels-seen) 1)
    
    ;; set up the i/o redirection hooks
    (setf (program-input (elt *progv* 0))
	  (lambda ()
	    ;; read value from hull
	    (let* ((key (format nil "~a ~a" x y))
		   (val (gethash key panels-seen)))
	      (if (null val)
		  (setf (gethash key panels-seen) 0)
		  val))))

    (let ((last-value-written nil)) ; "last-value-written" controls whether we interpret
				    ; the input as a value or a direction
      (setf (program-redirect-output-to (elt *progv* 0))
	    (lambda (value)
	      ;; write value to hull or move
	      (cond ((null last-value-written)
		     (setf last-value-written value))
		    (t			; NB 0 == left, 1 == right
		     (setf (gethash (format nil "~a ~a" x y) panels-seen) last-value-written)
		     (vprint 1 "~3a,~3a ~6a Write ~a rotate ~a~%" x y dir last-value-written (if (= 0 value) 'LEFT 'RIGHT))
		     (cond ((eql dir 'NORTH)
			    (setf dir (if (= 0 value) 'WEST 'EAST))
			    (incf x (if (= 0 value) -1 1)))
			   ((eql dir 'EAST)
			    (setf dir (if (= 0 value) 'NORTH 'SOUTH))
			    (incf y (if (= 0 value) 1 -1)))
			   ((eql dir 'SOUTH)
			    (setf dir (if (= 0 value) 'EAST 'WEST))
			    (incf x (if (= 0 value) 1 -1)))
			   (t
			    (setf dir (if (= 0 value) 'SOUTH 'NORTH))
			    (incf y (if (= 0 value) -1 1))))
		     (incf total-steps)
		     (if (> x max-x)
			 (setf max-x x))
		     (if (> y max-y)
			 (setf max-y y))
		     (if (< x min-x)
			 (setf min-x x))
		     (if (< y min-y)
			 (setf min-y y))
		     (setf last-value-written nil))))))
  
    (when (> *verbose* 1)
      (dump-prog-state "orig" (elt *progv* 0)))
      
    (do ((done nil))
	(done)
      (let ((result (run-intcode-program (elt *progv* 0))))
	(vprint 1 "Result[~a] is ~a~%" 0 result)
	(if (eql result 'HALT)
	    (setf done t))))

    (format t "Covered ~a panels in ~a steps. Extent: (~a,~a) - (~a,~a)~%"
	    (hash-table-count panels-seen) total-steps min-x min-y max-x max-y)

    (let ((map (make-array (list (1+ (- max-y min-y))))))
      (dotimes (y (length map))
	(setf (elt map y) (make-array (list (1+ (- max-x min-x))) :element-type 'character :initial-element #\#)))
	
      ;; (format t "empty map:~%")
      ;; (dotimes (y (length map))
      ;; 	(format t "~a~%" (elt map y)))

      (format t "making map~%")
      (loop for pos-str being the hash-keys in panels-seen using (hash-value paint-value)
	 do (multiple-value-bind (parsedok vals) (cl-ppcre:scan-to-strings "^(-?\\d+) (-?\\d+)" pos-str)
	      (when (not parsedok)
		(format t "bad position: ~a ~%" pos-str)
		(sb-ext:exit :code 1))
	      (let ((x (parse-integer (elt vals 0)))
		    (y (parse-integer (elt vals 1))))
		(format t "adding position ~a: = ~a,~a~%" pos-str x y)
		(format t "map[~a,~a] is currently [~a]~%" x y (elt (elt map (- y min-y)) (- x min-x)))
		(setf (elt (elt map (- y min-y)) (- x min-x))
		      (if (= paint-value 0) #\# #\Space)))))

      (loop for y downto 0 downfrom (1- (length map))
	do (format t "~a~%" (elt map y)))))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
