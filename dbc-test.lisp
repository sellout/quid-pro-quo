;;; dbc-test.lisp
;;;
;;; Tests for the dbc package.

;; (in-package "USER")

(defpackage "DBC-TEST"
  (:use "DBC" "CL")
  (:shadowing-import-from "DBC"
                          "DEFCLASS" "MAKE-INSTANCE" "DBC"))

(in-package "DBC-TEST")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgeneric test-dbc (arg1 arg2) (:method-combination
				  dbc
				  :invariant-check nil))
) ;; eval-when

(defmethod test-dbc ((m integer) (n integer))
  (print " >> test-dbc (integer integer)")
  (list m n))

(defmethod test-dbc :around ((m integer) (n integer))
  (print " >> test-dbc (around)")
  (call-next-method))

(defmethod test-dbc :precondition "fixnum > 123" ((m fixnum) (n integer))
  (print " >> precondition (fixnum integer)")
  (> m 123))

(defmethod test-dbc :precondition "fixnum > 100" ((m integer) (n fixnum))
  (print " >> precondition (integer fixnum)")
  (< n 100))

(defmethod test-dbc :precondition "integer =" ((m integer) (n integer))
  (print " >> precondition (integer integer)")
  (= m 12345678900987654321))

(defmethod test-dbc :postcondition "999" ((m integer) (n fixnum))
  (print " >> postcondition (integer fixnum)")
  999)

(defmethod test-dbc :postcondition "always true" ((m integer) (n integer))
  (print " >> postcondition (integer integer)")
  t)

(defmethod test-dbc :invariant "int invariant" ((m integer) (n integer))
  (print " >> invariant (integer integer)")
  'foo)

(defmethod test-dbc :invariant "int fix" ((m fixnum) (n integer))
  (print " >> invariant (fixnum integer)")
  'foo)

(defmethod test-dbc :invariant "fix fix" ((m integer) (n fixnum))
  (print " >> invariant (integer fixnum)")
  'foo)

(defmethod test-dbc :before ((m integer) (n integer))
  (print " >> before (integer integer)")
  (list (- m 1) (- n 1)))

(defmethod test-dbc :after ((m integer) (n integer))
  (print " >> after (integer integer)")
  (list (+ m 1) (+ n 1)))

#| Example:
(test-dbc 1 2)

;;; Fail:
(test-dbc 1 12345678900987654321)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass foo () 
  ((my-slot :accessor my-slot :initform nil)
   (your-slot :accessor your-slot :initform t))
  (:invariant (lambda (class) 
		(format t "~& >> Invariant check for class ~A~%"
			class)
		t)))

(defclass bar (foo) 
  ((yet-another-slot :accessor yet-another-slot :initform 'yas))
  (:invariant
   (lambda (class)
     (declare (ignore class))
     (format t " ++ Additional invariant (bar)~%")
     t)))
) ;; eval-when

(defmethod my-slot :precondition ((bar bar))
  (format t " ++ Additional precondition (my-slot bar)~%")
  t)

(defmethod my-slot :postcondition ((bar bar))
  (format t " ++ Additional postcondition (my-slot bar)~%")
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass bar-2 (foo)
  ()
  (:invariant (lambda (class)
		(declare (ignorable class))
		(format t "~& >> Strengthened invariant.~%")
		t)))
) ;; eval-when

#| Example:

(let* ((my-foo (make-instance 'foo))
       (a-slot (progn (format t " !! Accessing my-slot.~%")
		      (my-slot my-foo))))
  (setf (my-slot my-foo) (progn (format t " !! Setting my-slot.~%")
				9999))
  (list (my-slot my-foo) a-slot (your-slot my-foo)))

(let* ((my-bar (make-instance 'bar))
       (a-slot (progn (format t " !! Accessing my-slot.~%")
		      (my-slot my-bar))))
  (setf (my-slot my-bar) (progn (format t " !! Setting my-slot.~%")
				9999))
  (list (my-slot my-bar) a-slot (your-slot my-bar)))

(let* ((my-bar-2 (make-instance 'bar-2))
       (a-slot (progn (format t " !! Accessing my-slot.~%")
		      (my-slot my-bar-2))))
  (setf (my-slot my-bar-2) (progn (format t " !! Setting my-slot.~%")
				9999))
  (list (my-slot my-bar-2) a-slot (your-slot my-bar-2)))

(my-slot (make-instance 'bar))
(yet-another-slot (make-instance 'bar))

(my-slot (make-instance 'bar-2))

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass test () 
  ((my-slot :accessor my-slot :initarg :my-slot :initform 0))
  (:invariant
   "Invariant of test"
   (lambda (class)
     (numberp (slot-value class 'my-slot)))))

(defclass test-2 (test)
  ((another-slot :accessor another-slot :initarg :another-slot
		 :initform nil))
  (:invariant
   "Test-2 invariant"
   (lambda (class)
     (< (length (slot-value class 'another-slot))
	4))))
);

(defmethod test-dbc :around ((m test) (n test))
  (print " >> test-dbc (around)")
  (call-next-method))

(defmethod test-dbc ((m test) (n test))
  (print " >> test-dbc (test test)")
  (list m n))

(defmethod test-dbc :before ((m test) (n test))
  (print " >> before (test test)")
  (list m n 'before))

(defmethod test-dbc :after ((m test) (n test))
  (print " >> after (test test)")
  (list m n 'after))

;; Preconditions:

(defmethod test-dbc :precondition "< 123" ((m test-2) (n test))
  (print " >> precondition (test-2 test)")
  (< (my-slot m) 123))

(defmethod test-dbc :precondition "null" ((m test) (n test-2))
  (print " >> precondition (test test-2)")
  (null (another-slot n)))

(defmethod test-dbc :precondition "zerop" ((m test) (n test))
  (print " >> precondition (test test)")
  (not (zerop (my-slot m))))

;; Postconditions:

(defmethod test-dbc :postcondition "null another-slot"
      ((m test) (n test-2))
  (print " >> postcondition (test test-2)")
  (null (another-slot n)))

(defmethod test-dbc :postcondition  "zerop" ((m test) (n test))
  (print " >> postcondition (test test)")
  (or (zerop (my-slot m)) (zerop (my-slot n))))

(defmethod fail-invariant ((m test))
  (setf (my-slot m) nil))

#| Examples:

(test-dbc (make-instance 'test :my-slot 1) (make-instance 'test))

;;; Fail (precondition violation)
;;;
(test-dbc (make-instance 'test) (make-instance 'test))

;;; The next call succeeds because the method TEST-DBC has a weakened
;;; precondition for first arguments of type TEST-2.
;;;
(test-dbc (make-instance 'test-2) (make-instance 'test))

;;; Fail (postcondition violation)
;;;
(test-dbc (make-instance 'test :my-slot 1)
	  (make-instance 'test :my-slot 1))

;;; The weakened postcondition for second argument of class TEST-2
;;; does not cause the method to succeed.
;;;
(test-dbc (make-instance 'test :my-slot 1)
	  (make-instance 'test-2 :my-slot 1))

(make-instance 'test :my-slot -1)
(make-instance 'test :my-slot nil)

(fail-invariant (make-instance 'test))

|#

;;; End of file dbc-test.lisp
