(defpackage quid-pro-quo
  (:use #:closer-common-lisp #:closer-mop)
  (:nicknames #:qpq)
  (:export #:contract #:contracted-class
           #:contract-violation-error
           #:precondition-error #:postcondition-error
           #:invariant-error #:creation-invariant-error
           #:before-invariant-error #:after-invariant-error
           #:malformed-contract-warning #:overly-strict-precondition-warning))
