(in-package #:quid-pro-quo)

(defmacro defcontract (name type lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (let ((doc-symbol (when doc-string (intern doc-string)))
          (arglist (gensym "ARGLIST"))
          (fdefn (gensym "FDEFN")))
      `(progn
         (sb-int:unencapsulate ',name ',doc-symbol)
         (sb-int:encapsulate
          ',name ',doc-symbol
          '(let ((,arglist (eval 'sb-int:arg-list))
                 (,fdefn (eval 'sb-int:basic-definition)))
            (destructuring-bind ,lambda-list ,arglist
              ,@declarations
              ,(ecase type
                 (:require `(if (progn ,@remaining-forms)
                                (apply ,fdefn ,arglist)
                                (error 'precondition-error
                                       :failed-check (fdefinition ',name)
                                       :arguments ,arglist
                                       :description ,doc-string)))
                 (:guarantee (let ((%results (gensym "%RESULTS")))
                               `(let ((,%results nil))
                                  (flet ((results ()
                                           (values-list ,%results)))
                                    (ignore-errors
                                     (let ((*preparing-postconditions* t)
                                           (*inside-contract-p* t))
                                       ,@remaining-forms))
                                    (setf ,%results
                                          (multiple-value-list
                                           (apply ,fdefn ,arglist)))
                                    (or (let ((*inside-contract-p* t))
                                          ,@remaining-forms)
                                        (error 'postcondition-error
                                               :failed-check (fdefinition ',name)
                                               :arguments ,arglist
                                               :results (results)
                                               :description ,doc-string))
                                    (results)))))))))))))
