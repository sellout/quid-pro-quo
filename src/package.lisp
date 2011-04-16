(defpackage design-by-contract
  (:use #:closer-common-lisp #:closer-mop)
  (:nicknames #:dbc)
  (:export #:contract #:contracted-class
           #:contract-violation-error
           #:precondition-error #:postcondition-error
           #:invariant-error #:creation-invariant-error
           #:before-invariant-error #:after-invariant-error))
