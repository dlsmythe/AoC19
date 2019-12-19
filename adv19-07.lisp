;;; sbcl --noinform --load adv19-07.lisp [-v n] [-f input-file-name]
;;;  -v n  set verbosity to level n
;;;

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)
(ql:quickload "cl-permutation" :silent t)

(load "queues.lisp")

(defparameter *verbose* 0)
(defparameter *part* 1)
(defparameter *progfile* "adv19-07.input")

(defstruct program
  code
  input					; list
  redirect-input			; flag
  redirect-output-to)			; program-number

(defparameter *progv* (make-array '(5)))

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

(defun dump-program (tag program)
  (format t "~d prog:~%" tag)
  (loop for i below (length program)
       do (format t "~4,,,0@d: ~a~%" i (elt program i))))

(defun dump-prog-state ()
  (loop for i below 5 do
       (let ((pgm (elt *progv* i)))
	 (format t "Prog ~a: input ~a r-input ~a r-output ~a~%" i
		 (dls:members (program-input pgm))
		 (program-redirect-input pgm)
		 (program-redirect-output-to pgm)))))

;; ===========================================
	   
(defun input-number (prognum)
  (vprint 1 "input from prog ~a: avail ~a~%" prognum (dls:members (program-input (elt *progv* prognum))))
  (let* ((program (elt *progv* prognum))
	 (r (program-input program)))
    (cond ((null r)
	   (parse-integer (read-line *STANDARD-INPUT* nil)))
	  (t
	   (when (dls:empty r)
	    (format t "EOF on input program ~a~%" prognum)
	    (sb-ext:exit :code 1))
	   (dls:dequeue r)))))

(defun output-number (prognum value)
  (format t "output ~a ~d~%" prognum value)
  (let* ((program (elt *progv* prognum))
	 (redirection (program-redirect-output-to program)))
    (vprint 1 "output ~a from program ~a: redirection ~a~%" value prognum redirection)
    (cond ((null redirection)		; not redirected
	   (format t "OUTPUT: ~a~%" value))
	  ((numberp redirection)	; redirected to another numbered program
	   (vprint 1 "  redirected to program ~a~%" redirection)
	   (dls:enqueue (program-input (elt *progv* redirection)) value))
	  (t				; redirected to a list
	   (vprint 1 "  redirected to queue ~a~%" redirection)
	   (dls:enqueue redirection value)))))

(defun decode-opcode (prognum program pc)
  (let* ((op-str (format nil "~4@a" (elt program pc)))
	 (tmpstr (reverse op-str))
	 (opcode (parse-integer (nreverse (subseq (copy-seq tmpstr) 0 2))))
	 (op-0-immediate (char= (char tmpstr 2) #\1))
	 (op-1-immediate (char= (char tmpstr 3) #\1))
	 (tmsg (format nil " -- opcode ~a imm: ~a,~a: " opcode op-0-immediate op-1-immediate)))
    (cond ((= opcode 99)
	   (return-from decode-opcode (values opcode nil nil nil 1)))

	  ((or (= opcode 1) (= opcode 2) (= opcode 7) (= opcode 8)) ; +/*/less-than/equals
	   (let ((op-0-val (elt program (+ pc 1)))
		 (op-1-val (elt program (+ pc 2)))
		 (dest-pos (elt program (+ pc 3))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a ~a ~a" pc op-str op-0-val op-1-val dest-pos tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt program op-0-val))
		(if op-1-immediate op-1-val (elt program op-1-val))
		dest-pos
		4))))

	  ((= opcode 3)			; input
	   (let ((op-0-val (elt program (+ pc 1))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a" pc op-str op-0-val tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(input-number prognum)
		nil
		op-0-val
		2))))

	  ((= opcode 4)			; output
	   (let ((op-0-val (elt program (+ pc 1))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a" pc op-str op-0-val tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt program op-0-val))
		nil
		nil
		2))))

	  ((or (= opcode 5) (= opcode 6)) ; jump-if-true/jump-if-false
	   (let ((op-0-val (elt program (+ pc 1)))
		 (dest-pos (elt program (+ pc 2))))
	     (vprint 2 "DECODE[~a]: ~a ~a ~a ~a" pc op-str op-0-val dest-pos tmsg)
	     (return-from decode-opcode
	       (values
		opcode
		(if op-0-immediate op-0-val (elt program op-0-val))
		nil
		(if op-1-immediate dest-pos (elt program dest-pos))
		3))))

	   (t
	    (format t "Unsupported opcode: ~a~%" opcode)
	    (sb-ext:exit :code 1)))))

(defun run-intcode-program (prognum program)
  (do ((pc 0)
       (quit nil))
      (quit)
    (multiple-value-bind (opcode operand-0 operand-1 result-position pc-increment) (decode-opcode prognum program pc)
      (vprint 2 " op0: ~a op1: ~a destaddr: ~a pc-incr: ~a~%"
	      operand-0 operand-1 result-position pc-increment)
	
      (cond ((= 1 opcode)
	     (vprint 2 "TRACE: mem[~a] = ~a + ~a = ~a~%" result-position operand-0 operand-1 (+ operand-0 operand-1))
	     (setf (elt program result-position) (+ operand-0 operand-1))
	     (incf pc pc-increment))

	    ((= 2 opcode)
	     (vprint 2 "TRACE: mem[~a] = ~a * ~a = ~a~%" result-position operand-0 operand-1 (+ operand-0 operand-1))
	     (setf (elt program result-position) (* operand-0 operand-1))
	     (incf pc pc-increment))

	    ((= 3 opcode)
	     (vprint 2 "TRACE: INPUT mem[~a] = ~a~%" result-position operand-0)
	     (setf (elt program result-position) operand-0)
	     (incf pc pc-increment))

	    ((= 4 opcode)
	     (output-number prognum operand-0)
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
	     (setf (elt program result-position) (if (< operand-0 operand-1) 1 0))
	     (incf pc pc-increment))

	    ((= 8 opcode)
	     (vprint 2 "TRACE: SETEQ mem[~a] = ~a == ~a~%" result-position operand-0 operand-1)
	     (setf (elt program result-position) (if (= operand-0 operand-1) 1 0))
	     (incf pc pc-increment))
	     
	    ((= 99 opcode)
	     (setf quit t))

	    (t
	     (format t "Unexpected opcode ~a at pc ~a~%" (elt program pc) pc)
	     (return-from run-intcode-program 1))))))

(defparameter *perm-5-generator* (cl-permutation:make-perm-generator 5))

(defun main (args)

  ;; (do ((p (funcall *perm-5-generator*) (funcall *permstate*)))
  ;;     ((null p) (return-from main 1))
  ;;   (format t "perm: ~a~%" p)
  ;;   (loop for i below 5 do
  ;; 	 (format t "perm[~a] = ~a~%" p (cl-permutation:perm-ref p i))))
  
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

  (dotimes (i 5)
    (setf (elt *progv* i) (make-program))
    (setf (program-input (elt *progv* i)) (make-instance 'dls:queue)))

  ;; (format t "*progv* ~a: ~a ~a ~a ~a ~a~%" *progv*
  ;; 	  (sb-kernel:get-lisp-obj-address (elt *progv* 0))
  ;; 	  (sb-kernel:get-lisp-obj-address (elt *progv* 1))
  ;; 	  (sb-kernel:get-lisp-obj-address (elt *progv* 2))
  ;; 	  (sb-kernel:get-lisp-obj-address (elt *progv* 3))
  ;; 	  (sb-kernel:get-lisp-obj-address (elt *progv* 4)))
  
  (let ((orig-program (read-program *progfile*))
	(max-perm nil)
	(max-output 0))
    
    (dump-program "Original" orig-program)

    ;; for each phase permutation
    (do ((perm5 (funcall *perm-5-generator*) (funcall *perm-5-generator*))
	 (final-output (make-instance 'dls:queue) (make-instance 'dls:queue)))
	((null perm5))
      (format t "TRYING perm: ~a~%" perm5)

      ;; Set up the machines for this permutation of inputs
      (dotimes (i 5)
	(let ((program (elt *progv* i)))
	  ;; reinitialize the program memory
	  (setf (program-code program) (copy-seq orig-program))

	  ;; set up the redirections, according to the phase permutation
	  (setf (program-redirect-input program) t)
	  (if  (< i 4)
	       (setf (program-redirect-output-to program) (+ 1 i))
	       (setf (program-redirect-output-to program) final-output))

	  ;; add each program's phase value to its input queue
	  ;; (queue the corresponding permutation element to each program's input)
	  (dls:enqueue (program-input program) (1- (cl-permutation:perm-ref perm5 i)))))

      ;; Add the initial input value (0) to the first machine
      (dls:enqueue (program-input (elt *progv* 0)) 0)
	       
      (dump-prog-state)
      
      ;; run each program using this permutation of inputs
      (dotimes (i 5)
	(format t "Running program ~a: input ~a~%" i (program-input (elt *progv* i)))
	(run-intcode-program i (program-code (elt *progv* i))))

      ;; check the output
      (let ((outval (dls:dequeue final-output)))
	(vprint 1 "Perm output: ~a~%" outval)
	(when (> outval max-output)
	  (setf max-perm perm5)
	  (setf max-output outval))))

    (format t "Max value: ~a on permutation ~a~%" max-output max-perm))

  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
