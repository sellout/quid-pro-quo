(in-package #:quid-pro-quo)

(defgeneric function-name (check)
  (:method ((check method))
    (generic-function-name (method-generic-function check)))
  (:method ((check function))
    check))

(defun description (condition)
  (with-slots (failed-check description) condition
    (or description
        (or (second (method-qualifiers failed-check))
            (documentation failed-check t)))))

;;; We signal a condition of the appropriate type whenever a violation
;;; of the contract occurs.

(define-condition contract-violation-error (error)
  ((failed-check :initarg :failed-check)
   (arguments :initarg :arguments)
   (description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (with-slots (failed-check arguments) condition
               (format stream
                       "Contract violation on ~A called with ~A~@[: ~A~]."
                       (function-name failed-check)
                       arguments
                       (description condition)))))
  (:documentation "This is the superclass of all contract violations."))

(define-condition precondition-error (contract-violation-error)
  ()
  (:report (lambda (condition stream)
             (with-slots (failed-check arguments) condition
               (format stream
                       "The caller failed to meet the requirement~
                     ~:[s of~;~:* that ~A for~] ~A, when called with ~A."
                       (description condition)
                       (function-name failed-check)
                       arguments))))
  (:documentation
   "This error is signaled when a precondition fails. It is the caller's
    responsibility to fix whatever went wrong."))

(define-condition postcondition-error (contract-violation-error)
  ((results :initarg :results))
  (:report (lambda (condition stream)
             (with-slots (failed-check arguments results) condition
               (format stream
                       "~A failed to guarantee ~
                       ~:[its guarantees~;~:*the guarantee that ~A~], when ~
                        called with ~A, resulting in ~A."
                       (function-name failed-check)
                       (description condition)
                       arguments
                       results))))
  (:documentation
   "This error is signaled whenever a postcondition fails. It is the callee's
    responsibility to fix whatever is wrong."))

(define-condition invariant-error (contract-violation-error)
  ((object :initform nil :initarg :object))
  (:report (lambda (condition stream)
             (format stream "Invariant violation~@[ on ~A~]~@[:~& ~A~]."
                     (slot-value condition 'object) (description condition))))
  (:documentation
   "This error is signaled whenever an invariant fails. It is the class's
    responisibility to fix whatever is wrong."))

(define-condition before-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
             (with-slots (failed-check arguments object) condition
               (format stream
                       "Invariant violation~@[ on ~A~] before a call to ~A with~
                       ~A~@[:~& ~A~]."
                       object
                       (function-name failed-check)
                       arguments
                       (description condition)))))
  (:documentation
   "This invariant-error is signaled when the invariant fails when checked
    before a call."))

(define-condition after-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
             (with-slots (failed-check arguments object) condition
               (format stream
                       "Invariant violation~@[ on ~A~] after a call to ~A with~
                       ~A~@[:~& ~A~]."
                       object
                       (function-name failed-check)
                       arguments
                       (description condition)))))
  (:documentation
   "This invariant-error is signaled when the invariant fails when checked
    after a call."))

(define-condition creation-invariant-error (invariant-error)
  ()
  (:report (lambda (condition stream)
             (with-slots (arguments object) condition
               (format stream
                       "Invariant violation upon creation~@[ of ~A~] called~
                        with ~A~@[:~& ~A~]."
                       object arguments (description condition)))))
  (:documentation
   "This invariant-error is signaled when the invariant fails upon object
    creation."))

(define-condition malformed-contract-warning (warning)
  ((function :initarg :function)
   (arguments :initarg :arguments)
   (description :initarg :description :initform nil))
  (:report (lambda (condition stream)
             (with-slots (function arguments) condition
               (format stream
                       "The contract specified for ~A when called with ~A is~
                        not valid~@[: ~A~]."
                       function arguments (description condition)))))
  (:documentation
   "This warning is signaled when some discrepancy is identified in the contract
    itself."))

(define-condition overly-strict-precondition-warning
    (malformed-contract-warning)
  ((more-strict-method :initarg :more-strict-method)
   (less-strict-method :initarg :less-strict-method))
  (:report (lambda (condition stream)
             (with-slots
                   (function arguments more-strict-method less-strict-method)
                 condition
               (format stream
                       "A more-specific precondition (~A) on ~A is stricter~
                        than a less-specific precondition (~A) when called with~
                       ~A~@[: ~A~]."
                       more-strict-method
                       function
                       less-strict-method
                       arguments
                       (description condition)))))
  (:documentation
   "If there are multiple preconditions methods on a function, this warning is
    signaled when a more specific method is more restrictive than a less
    specific method. Preconditions may only remove restrictions as they get more
    specific."))
