(in-package #:quid-pro-quo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :qpq-precondition-checks *features*)
  (pushnew :qpq-postcondition-checks *features*)
  (pushnew :qpq-invariant-checks *features*))

(defparameter %results ()
  "Holds a list of values, accessed by the RESULTS function.")

(defun results ()
  "This is really only available to :ENSURE methods. It returns the values
   returned by the primary/around method, so they can be checked in the
   postcondition."
  (values-list %results))

(defparameter *inside-contract-p* nil)

(define-method-combination contract
    (&key (precondition-check t) (postcondition-check t) (invariant-check t))
  ((precondition (:require . *))
   (invariant (invariant . *))
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after))
   (postcondition (:ensure . *)))
  ;; NOTE: This gives us access to the object for invariant errors. Invariants
  ;;       only exist on readers, writers, and MAKE-INSTANCE. For writers, the
  ;;       object we care about is the second argument, in the other cases it's
  ;;       the first (and only). So here we grab the first two, and if it's a
  ;;       reader, WRITER-OBJECT will be nil.
  (:arguments reader-object writer-object)
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
           #+:qpq-precondition-checks
           (pre-form (if (and precondition-check
                              precondition
                              (not *inside-contract-p*))
                         `(let* ((contract-results
                                  (let ((*inside-contract-p* t))
                                    (list ,@(call-methods precondition))))
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
           #-:qpq-precondition-checks
           (pre-form around-form)
           #+:qpq-postcondition-checks
           (post-form (if (and postcondition-check
                               postcondition
                               (not *inside-contract-p*))
                          `(let ((%results (multiple-value-list ,pre-form))
                                 (*inside-contract-p* t))
                             ,@(apply #'call-methods
                                      postcondition
                                      (if (eq (method-generic-function
                                               (first primary))
                                              #'make-instance)
                                          (list 'creation-invariant-error
                                                :object reader-object)
                                          (list 'postcondition-error
                                                :method (first primary))))
                            (results))
                          pre-form))
           #-:qpq-postcondition-checks
           (post-form pre-form)
           #+:qpq-invariant-checks
           (inv-form (if (and invariant-check
                              invariant
                              (not *inside-contract-p*))
                         `(multiple-value-prog1
                              (progn
                                (let ((*inside-contract-p* t))
                                  ,@(call-methods invariant
                                                  'before-invariant-error
                                                  :object (or writer-object
                                                              reader-object)
                                                  :method (first primary)))
                                ,post-form)
                            (let ((*inside-contract-p* t))
                              ,@(call-methods invariant
                                              'after-invariant-error
                                              :object (or writer-object
                                                          reader-object)
                                              :method (first primary))))
                         post-form))
           #-:qpq-invariant-checks
           (inv-form post-form))
      inv-form)))
