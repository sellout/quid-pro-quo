(in-package #:quid-pro-quo)

(defmacro defcontract (name type description lambda-list &body body)
  `(progn
     (warn "Could not add contract to ~A: ~A does not seem to have any ability
            to wrap functions."
           ',name (lisp-implementation-type))
     (function ,name)))
