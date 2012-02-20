(in-package #:quid-pro-quo)

(defmacro defcontract (name type description &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well. The arguments to the original function are available via
   the ARGLIST variable. EG:

     (defcontract + :require \"all args < 10\"
       (every (lambda (n) (< n 10)) arglist)"
  `(ccl:advise ,name
               (or (progn ,@body)
                   (error ',(ecase type
                                   (:require 'precondition-error)
                                   (:ensure 'postcondition-error))
                          :method (function ,name)
                          :description ,description))
               :when ,(ecase type
                             (:require :before)
                             (:ensure :after))
               :name ,description))
