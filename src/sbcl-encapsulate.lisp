(in-package #:quid-pro-quo)

(defmacro sb-encapsulate (name identifier fdefn arglist &body body)
  (let ((old `(let ((,fdefn (eval (intern "BASIC-DEFINITION" "SB-INT")))
                    (,arglist (eval (intern "ARG-LIST" "SB-INT"))))
                ,@body))
        (new `(lambda (,fdefn &rest ,arglist) ,@body)))
    `(progn
       (sb-int:unencapsulate ',name ',identifier)
       (sb-int:encapsulate
        ',name ',identifier
        ,(cond
           ;; SBCL < 1.1.16 (before c89ad47)
           ((string= "BODY" (third (sb-introspect:function-lambda-list #'sb-int:encapsulate)))
            `',old)
           ;; SBCL = 1.1.16 (c89ad47 til c490156)
           ((eq :external (nth-value 1 (find-symbol "ARG-LIST" "SB-INT")))
            `(lambda () ,old))
           ;; SBCL > 1.1.16 (since c490156)
           (t new))))))

(defmacro defcontract (name type lambda-list &body body)
  "This macro makes it possible to add pre- and postconditions to non-generic
   functions as well."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (with-gensyms (fdefn arglist)
      `(sb-encapsulate ,name ,(when doc-string (intern doc-string))
           ,fdefn ,arglist
         (destructuring-bind ,lambda-list ,arglist
           ,@declarations
           ,(ecase type
              (:require `(if (progn ,@remaining-forms)
                             (apply ,fdefn ,arglist)
                             (error 'precondition-error
                                    :failed-check (fdefinition ',name)
                                    :arguments ,arglist
                                    :description ,doc-string)))
              (:guarantee (with-gensyms (contract)
                            `(flet ((,contract ()
                                      (let ((*inside-contract-p* t))
                                        ,@remaining-forms)))
                               (let ((*preparing-postconditions* t))
                                 (ignore-errors (,contract)))
                               (let ((%results (multiple-value-list
                                                (apply ,fdefn ,arglist))))
                                 (unless (,contract)
                                   (error 'postcondition-error
                                          :failed-check (fdefinition ',name)
                                          :arguments ,arglist
                                          :results %results
                                          :description ,doc-string))
                                 (values-list %results)))))))))))
