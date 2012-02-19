(defpackage quid-pro-quo
  (:use #:closer-common-lisp #:closer-mop)
  (:nicknames #:qpq)
  (:export #:contract #:contracted-class #:results #:old #:implies
           #:enable-contracts #:disable-contracts
           #:with-contracts-enabled #:with-contracts-disabled
           #:contract-violation-error
           #:precondition-error #:postcondition-error
           #:invariant-error #:creation-invariant-error
           #:before-invariant-error #:after-invariant-error
           #:malformed-contract-warning #:overly-strict-precondition-warning))

(in-package #:quid-pro-quo)

(defmacro implies (condition consequent)
  "A boolean operator that evaluates to the value of CONSEQUENT if CONDITION is
   true, and otherwise evaluates to T. This isn't particularly specific to Quid
   Pro Quo, but it is a useful logical operator for contracts."
  `(if ,condition ,consequent t))
