;;; dbc-test.lisp
;;;
;;; Tests for the dbc package.

;; (in-package "USER")

(defpackage "DBC-TEST"
  (:use "DBC" "CL" "FIVEAM")
  (:shadowing-import-from "DBC"
                          "DEFCLASS" "MAKE-INSTANCE" "DBC")
  (:export "TEST-DBC"))

(in-package "DBC-TEST")

(def-suite tests)

(in-suite tests)

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

(defmethod test-dbc :precondition ((m fixnum) (n integer))
  (print " >> precondition (fixnum integer)")
  (> m 123))

(defmethod test-dbc :precondition ((m integer) (n fixnum))
  (print " >> precondition (integer fixnum)")
  (< n 100))

(defmethod test-dbc :precondition ((m integer) (n integer))
  (print " >> precondition (integer integer)")
  (= m 12345678900987654321))

(defmethod test-dbc :postcondition ((m integer) (n fixnum))
  (print " >> postcondition (integer fixnum)")
  999)

(defmethod test-dbc :postcondition  ((m integer) (n integer))
  (print " >> postcondition (integer integer)")
  t)

(defmethod test-dbc :invariant  ((m integer) (n integer))
  (print " >> invariant (integer integer)")
  'foo)

(defmethod test-dbc :invariant  ((m fixnum) (n integer))
  (print " >> invariant (fixnum integer)")
  'foo)

(defmethod test-dbc :invariant  ((m integer) (n fixnum))
  (print " >> invariant (integer fixnum)")
  'foo)

(defmethod test-dbc :before ((m integer) (n integer))
  (print " >> before (integer integer)")
  (list (- m 1) (- n 1)))

(defmethod test-dbc :after ((m integer) (n integer))
  (print " >> after (integer integer)")
  (list (+ m 1) (+ n 1)))

(test should-succeed-with-integers
  (is (equal (list 1 2) (dbc-test:test-dbc 1 2))))

(test should-fail-n-<-100-precondition
  (signals precondition-error
    (dbc-test:test-dbc 1 12345678900987654321)))

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
(defclass test-1 () 
  ((my-slot :accessor my-slot :initarg :my-slot :initform 0))
  (:invariant
   "Invariant of test"
   (lambda (class)
     (numberp (slot-value class 'my-slot)))))

(defclass test-2 (test-1)
  ((another-slot :accessor another-slot :initarg :another-slot
		 :initform nil))
  (:invariant
   "Test-2 invariant"
   (lambda (class)
     (< (length (slot-value class 'another-slot))
	4))))
);

(defmethod test-dbc :around ((m test-1) (n test-1))
  (print " >> test-dbc (around)")
  (call-next-method))

(defmethod test-dbc ((m test-1) (n test-1))
  (print " >> test-dbc (test test)")
  (list m n))

(defmethod test-dbc :before ((m test-1) (n test-1))
  (print " >> before (test test)")
  (list m n 'before))

(defmethod test-dbc :after ((m test-1) (n test-1))
  (print " >> after (test test)")
  (list m n 'after))

;; Preconditions:

(defmethod test-dbc :precondition ((m test-2) (n test-1))
  (print " >> precondition (test-2 test)")
  (< (my-slot m) 123))

(defmethod test-dbc :precondition ((m test-1) (n test-2))
  (print " >> precondition (test test-2)")
  (null (another-slot n)))

(defmethod test-dbc :precondition ((m test-1) (n test-1))
  (print " >> precondition (test test)")
  (not (zerop (my-slot m))))

;; Postconditions:

(defmethod test-dbc :postcondition
      ((m test-1) (n test-2))
  (print " >> postcondition (test test-2)")
  (null (another-slot n)))

(defmethod test-dbc :postcondition ((m test-1) (n test-1))
  (print " >> postcondition (test test)")
  (or (zerop (my-slot m)) (zerop (my-slot n))))

(defmethod fail-invariant ((m test-1))
  (setf (my-slot m) nil))

(test should-succeed-with-test-objects
  (let ((first (make-instance 'test-1 :my-slot 1))
        (second (make-instance 'test-1)))
    (is (equal (list first second)
               (dbc-test:test-dbc first second)))))

(test should-fail-not-zerop-my-slot-precondition
  (let ((first (make-instance 'test-1))
        (second (make-instance 'test-1)))
    (signals precondition-error
      (dbc-test:test-dbc first second))))

(test should-pass-with-weakened-precondition
  (let ((first (make-instance 'test-2))
        (second (make-instance 'test-1)))
    ;; This succeeds because the method TEST-DBC has a weakened precondition for
    ;; first arguments of type TEST-2.
    (is (equal (list first second)
               (dbc-test:test-dbc first second)))))

(test should-fail-zerop-my-slot-postcondition
  (let ((first (make-instance 'test-1 :my-slot 1))
        (second (make-instance 'test-1 :my-slot 1)))
    (signals postcondition-error
      (dbc-test:test-dbc first second))))

(test should-fail-with-weakened-postcondition
  (let ((first (make-instance 'test-1 :my-slot 1))
        (second (make-instance 'test-2 :my-slot 1)))
    ;; The weakened postcondition for second argument of class TEST-2 does not
    ;; cause the method to succeed.
    (signals postcondition-error
      (dbc-test:test-dbc first second))))

(test should-create-successfully
  (is (typep (make-instance 'test-1 :my-slot -1)
             'test-1)))

(test should-fail-invariant-at-creation
  (signals creation-invariant-error
    (make-instance 'test-1 :my-slot nil)))

(test should-fail-invariant-after-method-call
  (signals after-invariant-error
    (fail-invariant (make-instance 'test-1))))

(test should-fail-invariant-after-setting-slot-value
  (signals after-invariant-error
    (setf (slot-value (make-instance 'test-1) 'my-slot) nil)))

(cl:defclass non-dbc-superclass ()
  ((foo :initform 10 :initarg :foo :accessor foo)))

(defclass dbc-subclass (non-dbc-superclass)
  ()
  (:invariant (lambda (instance)
                (> (slot-value instance 'foo) 5))))

(test should-fail-invariant-on-subclass-creation
  (signals creation-invariant-error
    (make-instance 'dbc-subclass :foo 5)))

(test should-fail-invariant-on-superclass-writer
  (let ((instance (make-instance 'dbc-subclass)))
    (signals after-invariant-error
      (setf (foo instance) 5))))

#| FIXME: currently this results in a stack overflow
(defclass inv-class ()
  ((foo :initform 10 :initarg :foo :accessor foo))
  (:invariant (lambda (instance)
                (> (foo instance) 5))))

(test should-not-recurse-on-reader-in-invariant
  (is (typep (make-instance 'inv-class) 'inv-class)))
|#

;;; End of file dbc-test.lisp
