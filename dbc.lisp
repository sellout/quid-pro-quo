(defpackage design-by-contract
  (:use #:cl)
  (:nicknames #:dbc)
  (:shadow #:defclass #:make-instance)
  (:export #:contract #:defclass #:make-instance
           #:contract-violation-error
           #:precondition-error #:postcondition-error
           #:invariant-error #:creation-invariant-error
           #:before-invariant-error #:after-invariant-error))

(in-package #:dbc)

;;; Enable all checks for testing purposes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :dbc-precondition-checks *features*)
  (pushnew :dbc-postcondition-checks *features*)
  (pushnew :dbc-invariant-checks *features*))

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
             (format stream "Contract violation~@[: ~A~]."
                     (description condition)))))

(define-condition precondition-error (contract-violation-error)
  ((method :reader method :initarg :method))
  (:report (lambda (condition stream)
             (format stream "Precondition violation on ~A~@[: ~A~]."
                     (method condition)
                     (description condition)))))

(define-condition postcondition-error (contract-violation-error)
  ((method :reader method :initarg :method))
  (:report (lambda (condition stream)
             (format stream "Postcondition violation on ~A~@[: ~A~]."
                     (method condition)
                     (description condition)))))

(define-condition invariant-error (contract-violation-error)
  ((object :initform nil :reader object :initarg :object)))

(define-condition before-invariant-error (invariant-error)
  ((method :reader method :initarg :method))
  (:report (lambda (condition stream)
	     (format stream
                     "Invariant violation ~@[on ~A ~]before ~A~@[:~% ~A~]."
                     (object condition)
                     (method condition)
		     (description condition)))))

(define-condition after-invariant-error (invariant-error)
  ((method :reader method :initarg :method))
  (:report (lambda (condition stream)
	     (format stream
                     "Invariant violation ~@[on ~A ~]after ~A~@[:~% ~A~]."
                     (object condition)
                     (method condition)
		     (description condition)))))

(define-condition creation-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
	     (format stream
                     "Invariant violation after creation of ~A~@[:~% ~A~]."
                     (object condition)
		     (description condition)))))

;;; The method combination CONTRACT.
;;; ===============================

(define-method-combination contract
  (&key (precondition-check t) (postcondition-check t) (invariant-check t))
  ((precondition (:precondition . *))
   (around (:around))
   (invariant (:invariant . *))
   (before (:before))
   (primary () :required t)
   (after (:after))
   (postcondition (:postcondition . *)))
  (labels ((call-methods (methods)
             (maplist (lambda (method-list)
                        `(call-method ,(car method-list) ,(cdr method-list)))
                      methods))
	   (raise-error (error-type methods &rest condition-parameters)
	     (maplist (lambda (method-list)
                        `(unless (call-method ,(car method-list)
                                              ,(cdr method-list))
                           (error ',error-type
                                  :description
                                  ,(second (method-qualifiers
                                            (car method-list)))
                                  ,@condition-parameters)))
                      methods)))
    (let* ((form (if (or before after (rest primary))
		     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                                 (call-method ,(first primary) ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary) ,(rest primary))))
	   (around-form (if around
                            `(call-method ,(first around)
                                          (,@(rest around) (make-method ,form)))
                            form))
	   #+:dbc-precondition-checks
	   (pre-form (if (and precondition-check precondition)
			 `(if (or ,@(call-methods precondition))
			      ,around-form
                              (progn
                                ,@(raise-error 'precondition-error
                                               precondition
                                               :method (first primary))))
                         around-form))
	   #-:dbc-precondition-checks
	   (pre-form around-form)
	   #+:dbc-postcondition-checks
	   (post-form (if (and postcondition-check postcondition)
                          `(multiple-value-prog1
                               ,pre-form
                             (unless (and ,@(call-methods postcondition))
                               ,@(raise-error 'postcondition-error
                                              postcondition
                                              :method (first primary))))
                          pre-form))
	   #-:dbc-postcondition-checks
	   (post-form pre-form)
	   #+:dbc-invariant-checks
	   (inv-form (if (and invariant-check invariant)
		         `(multiple-value-prog1
                              (progn
                                (unless (and ,@(call-methods invariant))
                                  ,@(raise-error 'before-invariant-error
                                                 invariant
                                                 :method (first primary)))
                                ,post-form)
                            (unless (and ,@(call-methods invariant))
                              ,@(raise-error 'after-invariant-error
                                             invariant
                                             :method (first primary))))
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
The generic functions have method combination type `contract'."
  (let ((accessor (getf (rest slot) :accessor)))
    (let ((reader (or (getf (rest slot) :reader) accessor))
          (writer (or (getf (rest slot) :writer)
                      (when accessor
                        `(setf ,accessor)))))
      (list (when reader
              `(ensure-generic-function ',reader
                                        :lambda-list '(object)
                                        :method-combination '(contract)))
            (when writer
              `(ensure-generic-function ',writer
                                        :lambda-list '(new-value object)
                                        :method-combination '(contract)))))))

(defun define-slot-accessor-invariants (class-name slot)
  "Returns a list with method definitions for reader and writer
invariants."
  (let ((accessor (getf (rest slot) :accessor)))
    (let ((reader (or (getf (rest slot) :reader) accessor))
          (writer (or (getf (rest slot) :writer)
                      (when accessor
                        `(setf ,accessor)))))
      (list (when reader
              `(defmethod ,reader :invariant ((object ,class-name))
                 (check-invariant object)))
            (when writer
              `(defmethod ,writer :invariant (value (object ,class-name))
                 (declare (ignore value))
                 (check-invariant object)))))))

(defun define-check-invariant-method (invariant class-name)
  "Returns a list containing the method on CHECK-INVARIANT specialized
for CLASS-NAME and executing INVARIANT."
  `((defmethod check-invariant ((object ,class-name))
      (when (funcall ,invariant object)
	(call-next-method)))))

(defmacro defclass (&body body)
  (destructuring-bind (name supers &optional slots &rest options) body
    (multiple-value-bind (invariant-form new-options)
        (getf-and-remove :invariant options)
      (let ((documented-invariant (cadr invariant-form)))
	(let ((invariant (or documented-invariant (car invariant-form))))
	  `(progn
	     ,@(if slots
		   (apply #'append
			  (mapcar (lambda (slot) (define-slot-generics slot))
				  slots))
                   '())
	     (cl:defclass ,name ,supers
               ,slots
               ,@new-options)
	     ,@(when invariant
                 (define-check-invariant-method invariant name))
	     ,@(when slots
                 (apply #'append
                        (mapcar (lambda (slot)
				  (define-slot-accessor-invariants name slot))
				slots)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric check-invariant (object)
    (:documentation
     "Methods on the generic `check-invariant' are used by the contract
method combination to perform the invariant check and should not
directly be defined by the user.")))

(defmethod check-invariant (object)
  "Default invariant, always true."
  (declare (ignore object))
  t)

(defmethod make-instance (class-name &rest initargs)
  (let ((object (apply #'cl:make-instance class-name initargs)))
    (unless (check-invariant object)
      (error 'creation-invariant-error :object object))
    object))
