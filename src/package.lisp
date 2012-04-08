(defpackage quid-pro-quo
  (:use #:closer-common-lisp #:closer-mop
        #:method-combination-utilities
        #:alexandria)
  (:nicknames #:qpq)
  (:export #:contract #:contracted-class #:funcallable-contracted-class
           #:results #:old #:implies
           #:defrequirement #:defguarantee
           #:enable-contracts #:disable-contracts
           #:with-contracts-enabled #:with-contracts-disabled
           #:contract-violation-error
           #:precondition-error #:postcondition-error
           #:invariant-error #:creation-invariant-error
           #:before-invariant-error #:after-invariant-error
           #:malformed-contract-warning #:overly-strict-precondition-warning
           ;; These are for extending invariant support to alternative object
           ;; systems or other extensions (see loom-slot-value-invariants.lisp
           ;; for an example).
           #:*invariant-initializers* #:add-invariant #:passes-invariants-p))

(in-package #:quid-pro-quo)

(defmacro implies (condition consequent)
  "A boolean operator that evaluates to the value of CONSEQUENT if CONDITION is
   true, and otherwise evaluates to T. This isn't particularly specific to Quid
   Pro Quo, but it is a useful logical operator for contracts."
  `(if ,condition ,consequent t))
