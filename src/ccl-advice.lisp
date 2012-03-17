(in-package #:quid-pro-quo)

(defmacro defcontract (name type lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    `(ccl:advise ,name
                 (destructuring-bind ,lambda-list ccl:arglist
                   ,@declarations
                   ,(ecase type
                           (:require `(or (progn ,@remaining-forms)
                                          (error 'precondition-error
                                                 :failed-check (function ,name)
                                                 :arguments ccl:arglist
                                                 :description ,doc-string)))
                           (:ensure (let ((%results (gensym)))
                                      `(let ((,%results nil))
                                         (flet ((results ()
                                                  (values-list ,%results)))
                                           (ignore-errors
                                             (let ((*preparing-postconditions* t)
                                                   (*inside-contract-p* t))
                                               ,@remaining-forms))
                                           (setf ,%results
                                                 (multiple-value-list (:do-it)))
                                           (or (let ((*inside-contract-p* t))
                                                 ,@remaining-forms)
                                               (error 'postcondition-error
                                                      :failed-check (function ,name)
                                                      :arguments ccl:arglist
                                                      :description ,doc-string))
                                           (results)))))))
                 :when ,(ecase type
                               (:require :before)
                               (:ensure :around))
                 :name ,doc-string)))
