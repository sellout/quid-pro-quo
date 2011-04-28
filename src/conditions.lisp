(in-package #:quid-pro-quo)

;;; We signal a condition of the appropriate type whenever a violation
;;; of the contract occurs.

(define-condition contract-violation-error (error)
  ((description :reader description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (format stream "Contract violation~@[: ~A~]."
                     (description condition)))))

(define-condition precondition-error (contract-violation-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "The caller failed to meet the requirement~
                      ~:[s of~;~:* that ~A for~] ~A."
                     (description condition) (condition-method condition)))))

(define-condition postcondition-error (contract-violation-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "~A failed to ensure ~
                      ~:[its guarantees~;~:*the guarantee that ~A~]."
                     (condition-method condition) (description condition)))))

(define-condition invariant-error (contract-violation-error)
  ((object :initform nil :reader object :initarg :object)))

(define-condition before-invariant-error (invariant-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream
                     "Invariant violation ~@[on ~A ~]before ~A~@[: ~A~]."
                     (object condition)
                     (condition-method condition)
                     (description condition)))))

(define-condition after-invariant-error (invariant-error)
  ((method :reader condition-method :initarg :method))
  (:report (lambda (condition stream)
             (format stream "Invariant violation ~@[on ~A ~]after ~A~@[: ~A~]."
                     (object condition)
                     (condition-method condition)
                     (description condition)))))

(define-condition creation-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invariant violation upon creation of ~A~@[: ~A~]."
                     (object condition) (description condition)))))

(define-condition malformed-contract-warning (warning)
  ((method :reader condition-method :initarg :method)
   (description :reader description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "The contract specified for ~A is not valid~@[: ~A~]."
                     (condition-method condition) (description condition)))))

(define-condition overly-strict-precondition-warning
    (malformed-contract-warning)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A more-specific precondition on ~A is stricter than a ~
                      less-specific precondition~@[: ~A~]."
                     (condition-method condition) (description condition)))))
