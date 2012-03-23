(in-package #:quid-pro-quo)

(defmacro defcontract (name type lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (declare (ignore declarations))
    (let ((fwname (gensym)))
      `(progn
         (def-fwrapper ,fwname ,lambda-list
           ,@(ecase type
                    (:require `((or (progn ,@remaining-forms)
                                    (error 'precondition-error
                                           :method (function ,name)
                                           :description ,doc-string))
                                (call-next-fwrapper)))
                    (:guarantee (let ((%results (gensym)))
                                  `((let ((,%results nil))
                                      (flet ((results ()
                                               (values-list ,%results)))
                                        (ignore-errors
                                          (let ((*preparing-postconditions* t)
                                                (*inside-contract-p* t))
                                            ,@remaining-forms))
                                        (setf ,%results
                                              (multiple-value-list
                                               (call-next-fwrapper)))
                                        (or (let ((*inside-contract-p* t))
                                              ,@remaining-forms)
                                            (error 'postcondition-error
                                                   :method (function ,name)
                                                   :description ,doc-string))
                                        (results))))))))
         (fwrap ',name ',(intern doc-string) ',fwname)))))
