;;; dbc.lisp

;;; Design by Contract in Common Lisp.
;;; =================================

;;; One of the outstanding features of the Eiffel language is that it
;;; supports a concept called Design by Contract.  A comprehensive
;;; description is given in the following books
;;;
;;; Object Oriented Software Construction, 2nd ed.
;;; Bertrand Meyer
;;; Prentice Hall PTR, 1997
;;; ISBN 0-13-629155-4
;;;
;;; Eiffel: The Language, 2nd ed.
;;; Bertrand Meyer
;;; Prentice Hall PTR, 1992
;;; ISBN ???
;;;
;;; but the key point of DBC is that the relationship between a class
;;; and its clients is specified by a contract: There are certain
;;; conditions that the caller of a method specialized on a class has
;;; to fulfill so that the method can do its job (the preconditions)
;;; and the method guarantees certain things after its completion (the
;;; postconditions).  Furthermore a class may have certain properties
;;; that are always true about that class; these properties are called
;;; invariants.
;;;
;;; This file contains an implementation of DBC for CLOS.  Pre- and
;;; postconditions as well as invariants are specified by qualified
;;; methods of type dbc; the usual before, after and around method
;;; combinations are available for these methods as well.
;;;
;;; Example:
;;; =======
;;;
;;; (defpackage "DBC-TEST"
;;;   (:use "DBC" "CL")
;;;   (:shadowing-import-from "DBC"
;;;                          "DEFCLASS" "MAKE-INSTANCE" "DBC"))
;;;
;;; (in-package "DBC-TEST")
;;;
;;; (defclass test () 
;;;   ((slot1 :accessor slot1 :initarg :slot1 :initform 0))
;;;   (:invariant (lambda (class) 
;;;	 	(format t "~& >> Invariant check for class ~A~%"
;;;		 	class)
;;;		( numberp (slot-value class 'slot1)))))
;;;
;;; (defgeneric test-dbc (arg1 arg2) (:method-combination
;;;                                   dbc
;;;				      :invariant-check nil))
;;; (defmethod test-dbc :precondition "first arg zero" ((m test) (n test))
;;;   (format t "~& >> precondition (test test)~%")
;;;   (not (zerop (slot1 m))))
;;; (defmethod test-dbc ((m test) (n test))
;;;   (/ (slot1 n) (slot1 m)))
;;;
;;; (test-dbc (make-instance 'test) (make-instance 'test))
;;; (test-dbc (make-instance 'test :slot1 2) (make-instance 'test :slot1 8))


;;; Authors and Copyright.
;;; =====================
;;;
;;; The code was written by Matthias Hoelzl (tc@gauss.muc.de) and is
;;; placed in the public domain.  Rainer Joswig added the package
;;; definition and MCL patches.  The most recent version of this file
;;; should be available at
;;;
;;;    <http://www.muc.de/~hoelzl/tools/dbc/dbc.lisp>.
;;;
;;; Thanks to Rainer Joswig for pointing out errors and sending me
;;; notes on the design as well as MCL patches and to Douglas Thomas
;;; Crosher who found a bug in the definition of the method qualifier
;;; patterns.
;;;
;;; WARNING: This code is still in its very early stages and not
;;; suitable for production use.
;;;
;;; Have fun,
;;;    Matthias


;;; Change Log.
;;; ==========
;;;
;;; 1999-08-03  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Removed the possibility to add a contract-specification-string
;;;    to pre- and postconditions for acl5 compatibility.
;;;  * Improved the output for pre- and postconditions without
;;;    descriptions.
;;; 1998-10-14  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Fixed the qualifiers to use dotted lists as required by the
;;;    standard (thanks to Douglas Thomas Crosher for setting me
;;;    straight on this).
;;;  * Disabled the keyword arguments for PCL based implementations
;;;    so that the code compiles on them.
;;;
;;; 1998-10-13  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Contract checks can now be disabled for each generic function
;;;    with the keyword arguments :precondition-check,
;;;    :postcondition-check and :invariant-check to the
;;;    :method-combination argument.  Unfortunately this breaks on
;;;    PCL-based implementations.
;;;
;;; 1998-10-12  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Defined error classes so that contract violations can be
;;;    recognized and caught.
;;;  * Changed method combination type to allow descriptions
;;;    of the contract which are stored in the condition that
;;;    is signalled when the contract is violated.
;;;  * Changed defclass to allow :invariant functions to be
;;;    preceeded by a descriptive string.
;;;  * Features :dbc-precondition-checks, :dbc-postcondition-checks
;;;    and :dbc-invariant-checks must be present for the checks to
;;;    be enabled at compile time.
;;;  * `define-slot-accessor-invariants' takes additional
;;;    description argument.
;;;  * `getf-and-remove' now returns the cdar of the found pair as
;;;    first argument and not the cadar.
;;;
;;; 1998-10-11  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Added default method for `check-invariant'.
;;;  * Removed `(ensure-generic-function 'check-invariant)' from
;;;    `define-check-invariant-method'.
;;;  * Changed method combination type of `check-invariant' to
;;;    standard method combination.
;;;
;;; 1998-10-11  Rainer Joswig
;;;  * Added package definition.
;;;  * Added MCL patches.
;;;
;;; 1998-10-07  Matthias Hoelzl  <tc@gauss.muc.de>
;;;  * Initial version.
;;;  * Changed handling of pre-/postconditions and invariants to
;;;    match Eiffel's behavior more closely.  Errors are now signalled
;;;    by the method combination.

(cl:defpackage "DESIGN-BY-CONTRACT"
  (:use "COMMON-LISP")
  (:nicknames "DBC")
  (:shadow cl:defclass cl:make-instance)
  (:export "DBC" "DEFCLASS" "MAKE-INSTANCE"))

(in-package "DBC")

;;; Enable all checks for testing purposes
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :dbc-precondition-checks cl:*features*)
  (pushnew :dbc-postcondition-checks cl:*features*)
  (pushnew :dbc-invariant-checks cl:*features*)
) ;; eval-when


;;; Conditions.
;;; ==========

;;; We signal a condition of the appropriate type whenever a violation
;;; of the contract occurs.
;;;
(define-condition contract-violation-error (error)
  ((description :reader description
		:initarg :description
		:initform "(no description available)"))
  (:report (lambda (condition stream)
	     (if (description condition)
		 (format stream "Contract violation: ~A."
			 (description condition))
	       (format stream "Contract violation.")))))

(define-condition precondition-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
	     (if (description condition)
		 (format stream "Precondition violation: ~A."
			 (description condition))
	       (format stream "Precondition violation.")))))

(define-condition postcondition-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
	     (if (description condition)
		 (format stream "Postcondition violation: ~A."
			 (description condition))
	       (format stream "Postcondition violation.")))))

(define-condition before-invariant-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Invariant violation before method call:~% ~A."
		     (description condition)))))

(define-condition after-invariant-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Invariant violation after method call:~% ~A."
		     (description condition)))))

(define-condition creation-invariant-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Invariant violation after class creation:~% ~A."
		     (description condition)))))
	     


;;; The method combination DBC.
;;; ==========================

(define-method-combination dbc
  #-:pcl
  (&key (precondition-check t) (postcondition-check t) (invariant-check t))
  #+:pcl
  ()
  ((precondition (:precondition . *))
   (around (:around))
   (invariant (:invariant . *))
   (before (:before))
   (primary () :required t)
   (after (:after))
   (postcondition (:postcondition . *)))
  (labels ((call-methods (methods)
            (maplist #'(lambda (method-list)
			 `(call-method ,(car method-list)
				       ,(cdr method-list)))
		    methods))
	   (raise-error (error-type methods)
	     (maplist #'(lambda (method-list)
			 `(unless (call-method ,(car method-list)
					       ,(cdr method-list))
			    (error ',error-type
				   :description
				   ,(second (method-qualifiers
					     (car method-list))))))
		     methods)))
    (let* ((form (if (or before after (rest primary))
		     `(multiple-value-prog1
		       (progn ,@(call-methods before)
			      (call-method ,(first primary)
					   ,(rest primary)))
		       ,@(call-methods (reverse after)))
		   `(call-method ,(first primary) ,(rest primary))))
	   (around-form (if around
		      `(call-method ,(first around)
				    (,@(rest around)
				       (make-method ,form)))
		    form))
	   #+:dbc-precondition-checks
	   (pre-form (if #-:pcl
			 (and precondition-check precondition)
		         #+:pcl
			 precondition
			 `(if (or ,@(call-methods precondition))
			      ,around-form
			    (progn
			      ,@(raise-error 'precondition-error
					     precondition)))
		       around-form))
	   #-:dbc-precondition-checks
	   (pre-form around-form)
	   #+:dbc-postcondition-checks
	   (post-form (if #-:pcl
			  (and postcondition-check postcondition)
			  #+:pcl
			  postcondition
			 `(multiple-value-prog1
			   ,pre-form
			   (unless (and ,@(call-methods postcondition))
			     ,@(raise-error 'postcondition-error
					    postcondition)))
			pre-form))
	   #-:dbc-postcondition-checks
	   (post-form pre-form)
	   #+:dbc-invariant-checks
	   (inv-form (if #-:pcl
			 (and invariant-check invariant)
		         #+:pcl
			 invariant
			 `(multiple-value-prog1
			   (progn
			     (unless (and ,@(call-methods
					     invariant))
			       ,@(raise-error
				  'before-invariant-error
				  invariant))
			     ,post-form)
			   (unless (and ,@(call-methods invariant))
			     ,@(raise-error
				'after-invariant-error
				invariant)))
		       post-form))
	   #-:dbc-invariant-checks
	   (inv-form post-form))
	   inv-form)))

(defun getf-and-remove (name list &optional acc)
  "Find NAME in the alist LIST.  Returns nil as first value if NAME is
not found, the valus associated with NAME otherwise.  The second value
returned is LIST with the first occurence of pair (NAME value)
removed."
  (if (null list)
    (values nil (reverse acc))
    (if (eql (caar list) name)
      (values (cdar list) (append (reverse acc) (rest list)))
      (getf-and-remove name (rest list) (cons (first list) acc)))))

(defun define-slot-generics (slot)
  "Returns a list with the reader and writer generic functions for a slot.
The generic functions have method combination type `dbc'."
  (let ((accessor (getf (rest slot) :accessor)))
    (let ((reader (or (getf (rest slot) :reader)
                      accessor))
          (writer (or (getf (rest slot) :writer)
                      (when accessor
                        `(setf ,accessor)))))
      (list (when reader
              `(ensure-generic-function
                ',reader
                :lambda-list '(object)
                :method-combination #-mcl '(dbc:dbc)
                #+mcl (ccl::%find-method-combination nil 'dbc nil)))
            (when writer
              `(ensure-generic-function
                ',writer
                :lambda-list '(new-value object)
                :method-combination #-mcl'(dbc:dbc)
                #+mcl (ccl::%find-method-combination nil 'dbc nil)))))))

(defun define-slot-accessor-invariants (class-name slot)
  "Returns a list with method definitions for reader and writer
invariants."
  (let ((accessor (getf (rest slot) :accessor)))
    (let ((reader (or (getf (rest slot) :reader)
                      accessor))
          (writer (or (getf (rest slot) :writer)
                      (when accessor
                        `(setf ,accessor)))))
      (list (when reader
              `(defmethod ,reader :invariant ((object ,class-name))
                 (check-invariant object)))
            (when writer
              `(defmethod ,writer :invariant
		     (value (object ,class-name))
                 (declare (ignore value))
                 (check-invariant object)))))))


(defun define-check-invariant-method (invariant class-name)
  "Returns a list containing the method on CHECK-INVARIANT specialized
for CLASS-NAME and executing INVARIANT."
  `((defmethod check-invariant ((object ,class-name))
      (when (funcall ,invariant object)
	(call-next-method)))))

(defmacro defclass (&body body)
  (destructuring-bind (name supers &optional slots &rest options)
                      body
    (multiple-value-bind (invariant-form new-options)
                         (getf-and-remove :invariant options)
      (let ((documented-invariant (cadr invariant-form)))
	(let ((invariant (or documented-invariant (car invariant-form))))
	  `(progn
	     ,@(if slots
		   (apply #'append
			  (mapcar (lambda (slot)
				    (define-slot-generics slot))
				  slots))
		 '())
	     (cl:defclass ,name ,supers ,slots
			  ,@new-options)
	     ,@(when invariant
		 (define-check-invariant-method invariant name))
	     ,@(when slots
		 (apply #'append
			(mapcar (lambda (slot)
				  (define-slot-accessor-invariants
				    name slot))
				slots)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defgeneric check-invariant (object)
  (:documentation
   "Methods on the generic `check-invariant' are used by the dbc
method combination to perform the invariant check and should not
directly be defined by the user."))
) ; eval-when

(defmethod check-invariant (object)
  "Default invariant, always true."
  (declare (ignore object))
  t)

(defmethod make-instance (class-name &rest initargs)
  (let ((object (apply #'cl:make-instance class-name initargs)))
    (unless (check-invariant object)
      (error 'creation-invariant-error))
    object))

;;; End of file dbc.lisp
