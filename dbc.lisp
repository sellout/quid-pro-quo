(defpackage design-by-contract
  (:use #:closer-common-lisp #:closer-mop)
  (:nicknames #:dbc)
  (:export #:contract #:contracted-class
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

(defclass contracted-class (standard-class)
  ((invariants :initform () :initarg :invariants
               :reader direct-class-invariants)))

(defmethod validate-superclass
    ((class contracted-class) (superclass standard-class))
  t)

(defgeneric effective-class-invariants (class)
  (:method ((class contracted-class))
    (apply #'append
           (direct-class-invariants class)
           (mapcar #'effective-class-invariants
                   (class-direct-superclasses class))))
  (:method (class)
    (declare (ignore class))
    nil))

(defun check-effective-invariants (object)
  (loop for invariant in (effective-class-invariants (class-of object))
     if (not (funcall invariant object))
     do (error 'creation-invariant-error
               :object object
               :description (documentation invariant 'function))))

(defun passes-class-invariants-p (object)
  (loop for invariant in (effective-class-invariants (class-of object))
     if (not (funcall invariant object))
     return nil
     finally (return t)))

(defun add-reader-invariant (reader class)
  (add-method (ensure-generic-function reader
                                       :lambda-list '(object)
                                       :method-combination '(contract))
              (make-instance 'standard-method
                             :qualifiers '(:invariant)
                             :lambda-list '(object)
                             :specializers (list class)
                             :function #'passes-class-invariants-p)))

(defun add-writer-invariant (writer class)
  (add-method (ensure-generic-function writer
                                       :lambda-list '(new-value object)
                                       :method-combination '(contract))
              (make-instance 'standard-method
                             :qualifiers '(:invariant)
                             :lambda-list '(new-value object)
                             :specializers (list (find-class t) class)
                             :function (lambda (new-value object)
                                         (declare (ignore new-value))
                                         (passes-class-invariants-p object)))))

(defmethod initialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants))
  ;; FIXME: need to do this for all slots, not just direct slots
  (let ((slots (class-direct-slots instance)))
    (mapc (lambda (reader) (add-reader-invariant reader instance))
          (reduce #'append (mapcar #'slot-definition-readers slots)))
    (mapc (lambda (writer) (add-writer-invariant writer instance))
          (reduce #'append (mapcar #'slot-definition-writers slots)))))

(defmethod reinitialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants)))

(defmethod make-instance ((class contracted-class) &rest initargs)
  (declare (ignorable initargs)) ; NOTE: not ignorable, but CCL complains
  (let ((object (call-next-method)))
    (check-effective-invariants object)
    object))
