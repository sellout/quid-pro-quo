(in-package #:quid-pro-quo)

(defun ensure-contracted-function (function-name lambda-list)
  "This both ensures that the method combination is correct as well as that the
   correct version of STANDARD-GENERIC-FUNCTION is used (for some
   implementations, Closer-MOP's STANDARD-GENERIC-FUNCTION is different from
   CL's)."
  (ensure-generic-function function-name
                           ;; FIXME: CCL & SBCL blow up if we try to
                           ;;        CHANGE-CLASS here.
                           #-(or ccl sbcl) :generic-function-class
                           #-(or ccl sbcl) 'standard-generic-function
                           :lambda-list lambda-list
                           :method-combination *contract-method-combination*))

(defun description<-remaining-forms (remaining-forms)
  (if (= 1 (length remaining-forms))
      (car remaining-forms)
      `(progn ,@remaining-forms)))

(defun strip-lambda-list (ll)
  (mapcar (lambda (var) (if (listp var) (car var) var)) ll))

(defmacro defrequirement (name (&rest lambda-list) &body body)
  "Adds a precondition to the NAMEd function. It can be either a generic or
   non-generic function. If it's the former, then use a specialized lambda list,
   otherwise use an ordinary lambda list. The docstring, if any, will be used in
   failure reports."
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body :documentation t)
    (declare (ignore declarations))
    (let* ((stripped-ll (strip-lambda-list lambda-list))
           (lambda-vars (remove-if (rcurry #'member
                                           '(&optional &rest
                                             &key &allow-other-keys &aux))
                                   stripped-ll))
           (method `(progn
                      (ensure-contracted-function ',name',stripped-ll)
                      (defmethod ,name
                          :require
                          ,(format nil "~A"
                            (or doc-string
                             (description<-remaining-forms
                              remaining-forms)))
                        ,lambda-list
                        (declare (ignorable ,@lambda-vars))
                        ,@body))))
      (if (every #'symbolp lambda-list)
          `(if (and (fboundp ',name)
                    (not (typep (fdefinition ',name) 'generic-function)))
               (defcontract ,name :require ,lambda-list
                 (declare (ignorable ,@lambda-vars))
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
    (declare (ignore declarations))
    (let* ((stripped-ll (strip-lambda-list lambda-list))
           (lambda-vars (remove-if (rcurry #'member
                                           '(&optional &rest
                                             &key &allow-other-keys &aux))
                                   stripped-ll))
           (method `(progn
                      (ensure-contracted-function ',name ',stripped-ll)
                      (defmethod ,name
                          :guarantee
                          ,(or doc-string
                            (format nil "~A"
                             (description<-remaining-forms
                              remaining-forms)))
                        ,lambda-list
                        (declare (ignorable ,@lambda-vars))
                        ,@body))))
      (if (every #'symbolp lambda-list)
          `(if (and (fboundp ',name)
                    (not (typep (fdefinition ',name) 'generic-function)))
               (defcontract ,name :guarantee ,lambda-list
                 (declare (ignorable ,@lambda-vars))
                 ,@body)
               ,method)
          method))))
