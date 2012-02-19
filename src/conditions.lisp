(in-package #:quid-pro-quo)

;;; We signal a condition of the appropriate type whenever a violation
;;; of the contract occurs.

(define-condition contract-violation-error (error)
  ((description :reader description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (format stream "Contract violation~@[: ~A~]."
                     (description condition))))
  (:documentation "This is the superclass of all contract violations."))

(define-condition precondition-error (contract-violation-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "The caller failed to meet the requirement~
                      ~:[s of~;~:* that ~A for~] ~A."
                     (description condition) (condition-method condition))))
  (:documentation
   "This error is signaled when a precondition fails. It is the caller's
    responsibility to fix whatever went wrong."))

(define-condition postcondition-error (contract-violation-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "~A failed to ensure ~
                      ~:[its guarantees~;~:*the guarantee that ~A~]."
                     (condition-method condition) (description condition))))
  (:documentation
   "This error is signaled whenever a postcondition fails. It is the callee`s
    responsibility to fix whatever is wrong."))

(define-condition invariant-error (contract-violation-error)
  ((object :initform nil :reader object :initarg :object))
  (:report (lambda (condition stream)
             (format stream "Invariant violation~@[ on ~A~]~@[:~& ~A~]."
                     (object condition) (description condition))))
  (:documentation
   "This error is signaled whenever an invariant fails. It is the class's
    responisibility to fix whatever is wrong."))

(define-condition before-invariant-error (invariant-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "Invariant violation~@[ on ~A~] before ~A~@[:~& ~A~]."
                     (object condition)
                     (condition-method condition)
                     (description condition))))
  (:documentation
   "This invariant-error is signaled when the invariant fails when checked
    before a call."))

(define-condition after-invariant-error (invariant-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream "Invariant violation~@[ on ~A~] after ~A~@[:~& ~A~]."
                     (object condition)
                     (condition-method condition)
                     (description condition))))
  (:documentation
   "This invariant-error is signaled when the invariant fails when checked
    after a call."))

(define-condition creation-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Invariant violation upon creation~@[ of ~A~]~@[:~& ~A~]."
                     (object condition) (description condition))))
  (:documentation
   "This invariant-error is signaled when the invariant fails upon object
    creation."))

(define-condition malformed-contract-warning (warning)
  ((method :reader condition-method :initarg :method)
   (description :reader description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "The contract specified for ~A is not valid~@[: ~A~]."
                     (condition-method condition) (description condition))))
  (:documentation
   "This warning is signaled when some discrepancy is identified in the contract
    itself."))

(define-condition overly-strict-precondition-warning
    (malformed-contract-warning)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A more-specific precondition on ~A is stricter than a ~
                      less-specific precondition~@[: ~A~]."
                     (condition-method condition) (description condition))))
  (:documentation
   "If there are multiple preconditions methods on a function, this warning is
    signaled when a more specific method is more restrictive than a less
    specific method. Preconditions may only remove restrictions as they get more
    specific."))
