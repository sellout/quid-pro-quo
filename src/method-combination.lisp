(in-package #:dbc)

;;; Enable all checks for testing purposes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :dbc-precondition-checks *features*)
  (pushnew :dbc-postcondition-checks *features*)
  (pushnew :dbc-invariant-checks *features*))

(define-method-combination contract
    (&key (precondition-check t) (postcondition-check t) (invariant-check t))
  ((precondition (:precondition . *))
   (around (:around))
   (invariant (invariant . *))
   (before (:before))
   (primary () :required t)
   (after (:after))
   (postcondition (:postcondition . *)))
  (labels ((call-methods
               (methods &optional error-type &rest condition-parameters)
             (mapcar (lambda (method)
                       (if error-type
                           `(unless (call-method ,method)
                              (error ',error-type
                                     :description
                                     ,(second (method-qualifiers method))
                                     ,@condition-parameters))
                           `(call-method ,method)))
                     methods)))
    (let* ((form (if (or before after (rest primary))
		     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                                 (call-method ,(first primary) ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary) ,(rest primary))))
	   (around-form (if around
                            `(call-method ,(first around)
                                          (,@(rest around) (make-method ,form)))
                            form))
	   #+:dbc-precondition-checks
	   (pre-form (if (and precondition-check precondition)
                         `(let* ((contract-results (list ,@(call-methods precondition)))
                                 (first-failure (position-if #'null
                                                             contract-results))
                                 (last-success (position-if-not #'null
                                                                contract-results
                                                                :from-end t)))
                            (when first-failure
                              (when (and last-success
                                         (< first-failure last-success))
                                (warn 'overly-strict-precondition-warning
                                      :method ,(first primary)))
                              (when (= first-failure 0)
                                (error 'precondition-error
                                       :description ,(second
                                                      (method-qualifiers
                                                       (first precondition)))
                                       :method ,(first primary))))
                            ,around-form)
                         around-form))
	   #-:dbc-precondition-checks
	   (pre-form around-form)
	   #+:dbc-postcondition-checks
	   (post-form (if (and postcondition-check postcondition)
                          `(multiple-value-prog1
                               ,pre-form
                             ,@(call-methods postcondition
                                             'postcondition-error
                                             :method (first primary)))
                          pre-form))
	   #-:dbc-postcondition-checks
	   (post-form pre-form)
	   #+:dbc-invariant-checks
	   (inv-form (if (and invariant-check invariant)
                         `(multiple-value-prog1
                              (progn
                                ,@(call-methods invariant
                                                'before-invariant-error
                                                :method (first primary))
                                ,post-form)
                             ,@(call-methods invariant
                                             'after-invariant-error
                                             :method (first primary)))
                         post-form))
	   #-:dbc-invariant-checks
	   (inv-form post-form))
      inv-form)))
