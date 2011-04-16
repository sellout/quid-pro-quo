(in-package #:design-by-contract)

;;; We signal a condition of the appropriate type whenever a violation
;;; of the contract occurs.

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
