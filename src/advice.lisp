(in-package #:quid-pro-quo)

(defmacro defcontract (name type description lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  `(ccl:advise ,name
               (destructuring-bind ,lambda-list ccl:arglist
                 ,(ecase type
                         (:require `(or (progn ,@body)
                                        (error 'precondition-error
                                               :method (function ,name)
                                               :description ,description)))
                         (:ensure (let ((%results (gensym)))
                                    `(let ((,%results nil))
                                       (flet ((results ()
                                                (values-list ,%results)))
                                         (ignore-errors
                                           (let ((*preparing-postconditions* t)
                                                 (*inside-contract-p* t))
                                             ,@body))
                                         (setf ,%results
                                               (multiple-value-list (:do-it)))
                                         (or (let ((*inside-contract-p* t))
                                               ,@body)
                                             (error 'postcondition-error
                                                    :method (function ,name)
                                                    :description ,description))
                                         (results)))))))
               :when ,(ecase type
                             (:require :before)
                             (:ensure :around))
               :name ,description))
