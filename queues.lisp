(defpackage :dls
  (:use :common-lisp)
  (:export :queue :print-object :dequeue :enqueue :contains :empty :members :nmembers))

(in-package :dls)

(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
	     ;; at least five elements, so print ellipsis
	     (format stream "(~{~S ~}... ~S)"
		     (subseq list 0 3) (first tail)))
	    ;; otherwise print whole list
	    (t (format stream "~:S" list))))))

(defgeneric dequeue (queue)
  (:documentation "Remove the next item from the front of the queue."))
(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defgeneric enqueue (queue item)
  (:documentation "Add an item to the end of the queue."))
(defmethod enqueue ((queue queue) new-item)
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
	    (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)

(defgeneric contains (queue item)
  (:documentation "Return whether an item is in the queue."))
(defmethod contains ((queue queue) item)
  (with-slots (list) queue
      (find item list)))

(defgeneric empty (queue)
  (:documentation "Return whether the queue is empty."))
(defmethod empty ((queue queue))
  (with-slots (list) queue
      (null list)))

(defgeneric members (queue))
(defmethod members ((queue queue))
  (slot-value queue 'list))

(defgeneric nmembers (q))
(defmethod nmembers ((q queue))
  (length (slot-value q 'list)))
