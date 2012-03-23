(in-package #:quid-pro-quo)

(defmacro defrequirement (name (&rest lambda-list) &body body)
  "Adds a precondition to the NAMEd function. It can be either a generic or
   non-generic function. If it's the former, then use a specialized lambda list,
   otherwise use an ordinary lambda list. The docstring, if any, will be used in
   failure reports."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (declare (ignore remaining-forms declarations))
    (let ((method `(progn
                     (ensure-generic-function ',name
                                              :method-combination
                                              *contract-method-combination*)
                     (defmethod ,name
                         :require ,@(when doc-string (list doc-string))
                         ,lambda-list
                         ,@body))))
      (if (every #'symbolp lambda-list)
          `(if (and (fboundp ',name)
                    (not (typep (fdefinition ',name) 'generic-function)))
               (defcontract ,name :require ,lambda-list
                 ,@body)
               ,method)
          method))))

(defmacro defguarantee (name (&rest lambda-list) &body body)
  "Adds a postcondition to the NAMEd function. It can be either a generic or
   non-generic function. If it's the former, then use a specialized lambda list,
   otherwise use an ordinary lambda list. The docstring, if any, will be used in
   failure reports."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (declare (ignore remaining-forms declarations))
    (let ((method `(progn
                     (ensure-generic-function ',name
                                              :method-combination
                                              *contract-method-combination*)
                     (defmethod ,name
                         :guarantee ,@(when doc-string (list doc-string))
                         ,lambda-list
                         ,@body))))
      (if (every #'symbolp lambda-list)
          `(if (and (fboundp ',name)
                    (not (typep (fdefinition ',name) 'generic-function)))
               (defcontract ,name :guarantee ,lambda-list
                 ,@body)
               ,method)
          method))))
