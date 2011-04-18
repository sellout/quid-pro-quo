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
  (labels ((call-methods (methods)
             (mapcar (lambda (method) `(call-method ,method)) methods))
	   (raise-error (error-type methods &rest condition-parameters)
	     (mapcar (lambda (method)
                       `(unless (call-method ,method)
                          (error ',error-type
                                 :description
                                 ,(second (method-qualifiers method))
                                 ,@condition-parameters)))
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
			 `(if (or ,@(call-methods precondition))
			      ,around-form
                              (progn
                                ,@(raise-error 'precondition-error
                                               precondition
                                               :method (first primary))))
                         around-form))
	   #-:dbc-precondition-checks
	   (pre-form around-form)
	   #+:dbc-postcondition-checks
	   (post-form (if (and postcondition-check postcondition)
                          `(multiple-value-prog1
                               ,pre-form
                             (unless (and ,@(call-methods postcondition))
                               ,@(raise-error 'postcondition-error
                                              postcondition
                                              :method (first primary))))
                          pre-form))
	   #-:dbc-postcondition-checks
	   (post-form pre-form)
	   #+:dbc-invariant-checks
	   (inv-form (if (and invariant-check invariant)
                         `(multiple-value-prog1
                              (progn
                                (unless (and ,@(call-methods invariant))
                                  ,@(raise-error 'before-invariant-error
                                                 invariant
                                                 :method (first primary)))
                                ,post-form)
                            (unless (and ,@(call-methods invariant))
                              ,@(raise-error 'after-invariant-error
                                             invariant
                                             :method (first primary))))
                         post-form))
	   #-:dbc-invariant-checks
	   (inv-form post-form))
      inv-form)))
