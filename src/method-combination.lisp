(in-package #:quid-pro-quo)

#|
(defun enabled-contracts ()
  (intersection '(:qpq-invariant-checks
                  :qpq-precondition-checks
                  :qpq-postcondition-checks)
                *features*))

(defun set-contract-state (type onp)
  (if onp
      (pushnew type *features*)
      (setf *features* (remove type *features*))))

(defun enable-contracts
    (&key (invariants t) (preconditions t) (postconditions t))
  (set-contract-state :qpq-invariant-checks invariants)
  (set-contract-state :qpq-precondition-checks preconditions)
  (set-contract-state :qpq-postcondition-checks postconditions))

(defun disable-contracts ()
  (enable-contracts :invariants nil :preconditions nil :postconditions nil))

(defmacro with-contracts-enabled
    ((&rest args &key (invariants t) (preconditions t) (postconditions t))
     &body body)
  (let ((enabled-contracts (gensym "ENABLED-CONTRACTS")))
    `(let ((,enabled-contracts (enabled-contracts)))
       (unwind-protect
            (progn (enable-contracts ,@args)
                   ,@body)
         (disable-contracts)
         (mapcar (lambda (type) (set-contract-state type t))
                 ,enabled-contracts)))))

(defmacro with-contracts-disabled (() &body body)
  (let ((enabled-contracts (gensym "ENABLED-CONTRACTS")))
    `(let ((,enabled-contracts (enabled-contracts)))
       (unwind-protect
            (progn (disable-contracts)
                   ,@body)
         (mapcar (lambda (type) (set-contract-state type t))
                 ,enabled-contracts)))))

;;; Enable all checks for testing purposes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-contracts))
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :qpq-precondition-checks *features*)
  (pushnew :qpq-postcondition-checks *features*)
  (pushnew :qpq-invariant-checks *features*))

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
	   #+:qpq-precondition-checks
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
	   #-:qpq-precondition-checks
	   (pre-form around-form)
	   #+:qpq-postcondition-checks
	   (post-form (if (and postcondition-check postcondition)
                          `(multiple-value-prog1
                               ,pre-form
                             ,@(call-methods postcondition
                                             'postcondition-error
                                             :method (first primary)))
                          pre-form))
	   #-:qpq-postcondition-checks
	   (post-form pre-form)
	   #+:qpq-invariant-checks
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
	   #-:qpq-invariant-checks
	   (inv-form post-form))
      inv-form)))
