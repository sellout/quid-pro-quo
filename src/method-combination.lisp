(in-package #:quid-pro-quo)

(format *error-output* 
"~&;  NOTE: Quid Pro Quo was compiled with preconditions ~A, postconditions~@
   ;        ~A, and invariants ~A. If you wish to change this, you must add~@
   ;        or remove :QPQ-*-CHECKS-DISABLED in *FEATURES* and recompile the~@
   ;        system.~%"
        #+qpq-precondition-checks-disabled 'off
        #-qpq-precondition-checks-disabled 'on
        #+qpq-postcondition-checks-disabled 'off
        #-qpq-postcondition-checks-disabled 'on
        #+qpq-invariant-checks-disabled 'off
        #-qpq-invariant-checks-disabled 'on)

(defvar *check-invariants-p* t)
(defvar *check-preconditions-p* t)
(defvar *check-postconditions-p* t)

(defun enabled-contracts ()
  "Returns a list of arguments suitable to APPLYing to ENABLE-CONTRACTS."
  (list :invariants
        #+qpq-invariant-checks-disabled nil
        #-qpq-invariant-checks-disabled *check-invariants-p*
        :preconditions
        #+qpq-precondition-checks-disabled nil
        #-qpq-precondition-checks-disabled *check-preconditions-p*
        :postconditions
        #+qpq-postcondition-checks-disabled nil
        #-qpq-postcondition-checks-disabled *check-postconditions-p*))

(defun enable-contracts
    (&key (invariants nil invp)
          (preconditions nil prep)
          (postconditions nil postp))
  "Enables or disables each contract type that is provided. If none is provided,
   no change is made."
  (when invp (setf *check-invariants-p* invariants))
  (when prep (setf *check-preconditions-p* preconditions))
  (when postp (setf *check-postconditions-p* postconditions)))

(defun disable-contracts ()
  "A shorthand for disabling all contracts."
  (enable-contracts :invariants nil :preconditions nil :postconditions nil))

(defmacro with-contracts-enabled
    ((&rest args &key invariants preconditions postconditions) &body body)
  "Enables/disables contracts for the extent of this form, restoring them to
   their prior values upon exit."
  (declare (ignore invariants preconditions postconditions))
  (let ((enabled-contracts (gensym "ENABLED-CONTRACTS-")))
    `(let ((,enabled-contracts (enabled-contracts)))
       (unwind-protect
            (progn (enable-contracts ,@args)
                   ,@body)
         (apply #'enable-contracts ,enabled-contracts)))))

(defmacro with-contracts-disabled (() &body body)
  "A shorthand for disabling all contracts for the extent of this form."
  (let ((enabled-contracts (gensym "ENABLED-CONTRACTS-")))
    `(let ((,enabled-contracts (enabled-contracts)))
       (unwind-protect
            (progn (disable-contracts)
                   ,@body)
         (apply #'enable-contracts ,enabled-contracts)))))

(defparameter %results ()
  "Holds a list of values, accessed by the RESULTS function.")

(defun results ()
  "This is really only available to postconditions. It returns the values
   returned by the primary/around method, so they can be checked in the
   postcondition."
  (values-list %results))

(defparameter *preparing-postconditions* nil
  "This is true when we are setting up postconditions by pre-evaluating any
   forms that need an OLD value.")

(defparameter *postcondition-values* ()
  "This contains all the values computed for the current set of postconditions")

(defmacro old (expression)
  "Only available in postconditions, OLD evaluates its expression before the
   primary method is executed and stores it for use in the postcondition."
  (let ((value (gensym)))
    `(if *preparing-postconditions*
         (let ((,value ,expression))
           (push ,value *postcondition-values*)
           ,value)
         (pop *postcondition-values*))))

(defparameter *inside-contract-p* nil)

(define-method-combination contract
    (&key (invariant-check *check-invariants-p*)
          (precondition-check *check-preconditions-p*)
          (postcondition-check *check-postconditions-p*))
  ((invariant (invariant . *))
   (precondition (:require . *))
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after) :order :most-specific-last)
   (postcondition (:guarantee . *)))
  ;; NOTE: This gives us access to the object for invariant errors. Invariants
  ;;       only exist on slot accessors. For writers, the object we care about
  ;;       is the second argument, in the other cases it's the first (and only).
  ;;       So here we grab the first two, and if it's a reader, WRITER-OBJECT
  ;;       will be nil.
  (:arguments &whole whole reader-object writer-object)
  (:generic-function gf)
  "This method combination extends the STANDARD method combination by adding
  :REQUIRE and :GUARANTEE methods for pre- and postconditions, respectively. It
   also provides invariants, which are created automatically on slot-accessors
   for classes that use the CONTRACTED-CLASS metaclass. Invariant methods should
   not be created explicitly."
  (declare (ignore #+qpq-precondition-checks-disabled precondition-check
                   #+qpq-precondition-checks-disabled precondition
                   #+qpq-postcondition-checks-disabled postcondition-check
                   #+qpq-postcondition-checks-disabled postcondition
                   #+qpq-invariant-checks-disabled invariant-check
                   #+qpq-invariant-checks-disabled invariant))
  (flet ((test-methods
             (methods error-type
              &rest condition-parameters &key &allow-other-keys)
           (mapcar (lambda (method)
                     (unless (getf condition-parameters :description)
                       (setf (getf condition-parameters :description)
                             (or (second (method-qualifiers method))
                                 (documentation method t))))
                     ;; FIXME: CMUCL errors if this CALL-METHOD form doesn't
                     ;;        have a second argument for some reason.
                     `(unless (call-method ,method #+cmucl ())
                        (error ',error-type
                               :failed-check ,method
                               :arguments ,whole
                               ,@condition-parameters)))
                   methods))
         (prepare-postconditions (methods)
           (mapcar (lambda (method) `(ignore-errors (call-method ,method)))
                   (reverse methods))))
    (let* (;; FIXME: ACTUAL-GF is a workaround for implementations with a broken
           ;;       :GENERIC-FUNCTION option
           (actual-gf (or gf (method-generic-function (first primary))))
           (std-form (combine-standard-methods primary around before after))
           #+:qpq-precondition-checks-disabled
           (pre-form std-form)
           #-:qpq-precondition-checks-disabled
           (pre-form (if (and precondition-check
                              precondition
                              (not *inside-contract-p*))
                         `(progn
                            (when *check-preconditions-p*
                              (let* ((contract-results
                                      (let ((*inside-contract-p* t))
                                        (list ,@(call-methods precondition))))
                                     (first-failure (position-if
                                                     #'null
                                                     contract-results))
                                     (last-success (position-if-not
                                                    #'null
                                                    contract-results
                                                    :from-end t)))
                                (when first-failure
                                  (when (and last-success
                                             (< first-failure last-success))
                                    (warn 'overly-strict-precondition-warning
                                          :function ,actual-gf
                                          :arguments ,whole
                                          :more-strict-method (nth first-failure
                                                                   ',precondition)
                                          :less-strict-method (nth last-success
                                                                   ',precondition)))
                                  (when (= first-failure 0)
                                    (error 'precondition-error
                                           :failed-check ,(first precondition)
                                           :arguments ,whole)))))
                            ,std-form)
                         std-form))
           #+:qpq-postcondition-checks-disabled
           (post-form pre-form)
           #-:qpq-postcondition-checks-disabled
           (post-form (if (and postcondition-check
                               postcondition
                               (not *inside-contract-p*))
                          `(if *check-postconditions-p*
                               (progn
                                 ,@(unless (find actual-gf
                                                 (list #'make-instance
                                                       #'initialize-instance))
                                     `((let ((*preparing-postconditions* t)
                                             (*inside-contract-p* t))
                                         ,@(prepare-postconditions postcondition))))
                                 (let ((%results (multiple-value-list ,pre-form))
                                       (*inside-contract-p* t))
                                   ,@(apply #'test-methods
                                            postcondition
                                            (if (find actual-gf
                                                      (list #'make-instance
                                                            #'initialize-instance))
                                                (list 'creation-invariant-error
                                                      :object reader-object
                                                      :description
                                                      `(invariant-description
                                                        ,reader-object))
                                                (list 'postcondition-error
                                                      :results %results)))
                                   (results)))
                               ,pre-form)
                          pre-form))
           #+:qpq-invariant-checks-disabled
           (inv-form post-form)
           #-:qpq-invariant-checks-disabled
           (inv-form (if (and invariant-check
                              invariant
                              (not *inside-contract-p*))
                         `(if *check-invariants-p*
                              (multiple-value-prog1
                                  (progn
                                    (let ((*inside-contract-p* t))
                                      ,@(test-methods
                                         invariant
                                         'before-invariant-error
                                         :object `(or ,writer-object
                                                      ,reader-object)
                                         :description
                                         `(invariant-description
                                           (class-of (or ,writer-object
                                                         ,reader-object)))))
                                    ,post-form)
                                (let ((*inside-contract-p* t))
                                  ,@(test-methods invariant
                                                  'after-invariant-error
                                                  :object `(or ,writer-object
                                                               ,reader-object)
                                                  :description
                                                  `(invariant-description
                                                    (class-of (or ,writer-object
                                                                  ,reader-object))))))
                              ,post-form)
                         post-form)))
      (declare (ignore #+qpq-precondition-checks-disabled actual-gf))
      ;; NOTE: This LET form is a workaround because there's no way to specify
      ;;       IGNORABLE in the right place, so we use the outer variables to
      ;;       bind the inner ones, then we can declare the inner ones as
      ;;       ignorable.
      `(let ((reader-object ,reader-object)
             (writer-object ,writer-object))
         (declare (ignorable reader-object writer-object))
         ,inv-form))))

(defvar *contract-method-combination*
  #-(or allegro cmucl)
  (find-method-combination (class-prototype
                            (find-class 'standard-generic-function))
                           'contract
                           '())
  #+(or allegro cmucl) '(contract))

(defmethod documentation :around ((x standard-generic-function) doc-type)
  (declare (ignore doc-type))
  (format nil "~@[~A~]~@[~&guarantees:~%* ~A~]"
          (call-next-method)
          (let ((method (find-if (lambda (method)
                                   (and (eq :guarantee
                                            (car (method-qualifiers method)))
                                        (every (lambda (specializer)
                                                 (eq (find-class t)
                                                     specializer))
                                               (method-specializers method))))
                                 (generic-function-methods x))))
            (when method (second (method-qualifiers method))))))

(defmethod documentation :around ((x standard-method) doc-type)
  (declare (ignore doc-type))
  (let ((applicable-methods
         (compute-applicable-methods-using-classes (method-generic-function x)
                                                   (method-specializers x))))
    (let ((precondition (find :require applicable-methods
                              :key (compose #'car #'method-qualifiers)))
          (postconditions (remove :guarantee applicable-methods
                                  :test-not #'eq
                                  :key (compose #'car #'method-qualifiers))))
      (format nil "~@[~A~]~@[~&requires:~%* ~A~]~@[~&guarantees:~{~&* ~A~}~]"
              (call-next-method)
              (when precondition (second (method-qualifiers precondition)))
              (remove nil
                      (mapcar (compose #'second #'method-qualifiers)
                              postconditions))))))
