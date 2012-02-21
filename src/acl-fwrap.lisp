(in-package #:quid-pro-quo)

(defmacro defcontract (name type description lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  (let ((fwname (gensym)))
    `(progn
       (def-fwrapper ,fwname ,@arglist
         ,@(ecase type
                  (:require `((or (progn ,@body)
                                  (error 'precondition-error
                                         :method (function ,name)
                                         :description ,description))
                              (call-next-fwrapper)))
                  (:ensure (let ((%results (gensym)))
                             `((let ((,%results nil))
                                 (flet ((results ()
                                          (values-list ,%results)))
                                   (ignore-errors
                                     (let ((*preparing-postconditions* t)
                                           (*inside-contract-p* t))
                                       ,@body))
                                   (setf ,%results
                                         (multiple-value-list
                                          (call-next-fwrapper)))
                                   (or (let ((*inside-contract-p* t))
                                         ,@body)
                                       (error 'postcondition-error
                                              :method (function ,name)
                                              :description ,description))
                                   (results))))))))
       (fwrap ',name ',(intern description) ',fwname))))
