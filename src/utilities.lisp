(in-package #:quid-pro-quo)

(defmacro implies (condition consequent)
  "A boolean operator that evaluates to the value of CONSEQUENT if CONDITION is
   true, and otherwise evaluates to T. This isn't particularly specific to Quid
   Pro Quo, but it is a useful logical operator for contracts."
  `(if ,condition ,consequent t))
