(in-package #:quid-pro-quo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :qpq-precondition-checks *features*)
  (pushnew :qpq-postcondition-checks *features*)
  (pushnew :qpq-invariant-checks *features*))

(defvar *check-invariants-p* t)
(defvar *check-preconditions-p* t)
(defvar *check-postconditions-p* t)

(defun enabled-contracts ()
  "Returns a list of arguments suitable to APPLYing to ENABLE-CONTRACTS."
  (list :invariants *check-invariants-p*
        :preconditions *check-preconditions-p*
        :postconditions *check-postconditions-p*))

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
  "This is really only available to :ENSURE methods. It returns the values
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
   (after (:after))
   (postcondition (:ensure . *)))
  ;; NOTE: This gives us access to the object for invariant errors. Invariants
  ;;       only exist on slot accessors. For writers, the object we care about
  ;;       is the second argument, in the other cases it's the first (and only).
  ;;       So here we grab the first two, and if it's a reader, WRITER-OBJECT
  ;;       will be nil.
  (:arguments reader-object writer-object)
  (:generic-function gf)
  "This method combination extends the STANDARD method combination by adding
  :require and :ensure methods for pre- and postconditions, respectively. It
   also provides invariants, which are created automatically on slot-accessors
   for classes that use the CONTRACTED-CLASS metaclass. Invariant methods should
   not be created explicitly."
  (labels ((call-methods
               (methods &optional error-type &rest condition-parameters)
             (mapcar (lambda (method)
                       (if error-type
                           `(unless (call-method ,method)
                              ,(unless (getf condition-parameters :description)
                                 (setf (getf condition-parameters :description)
                                       (or (second (method-qualifiers method))
                                           (documentation method t))))
                              (error ',error-type ,@condition-parameters))
                           `(call-method ,method)))
                     methods))
           (prepare-postconditions (methods)
             (mapcar (lambda (method)
                       `(ignore-errors (call-method ,method)))
                     (reverse methods))))
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
                                          :method ,(first primary)))
                                  (when (= first-failure 0)
                                    (error 'precondition-error
                                           :description ,(second
                                                          (method-qualifiers
                                                           (first precondition)))
                                           :method ,(first primary))))))
                            ,around-form)
                         around-form))
           #-:qpq-precondition-checks
           (pre-form around-form)
           #+:qpq-postcondition-checks
           (post-form (if (and postcondition-check
                               postcondition
                               (not *inside-contract-p*))
                          `(if *check-postconditions-p*
                               (progn
                                 (unless ,(find gf
                                                (list #'make-instance
                                                      #'initialize-instance))
                                   (let ((*preparing-postconditions* t)
                                         (*inside-contract-p* t))
                                     ,@(prepare-postconditions postcondition)))
                                 (let ((%results (multiple-value-list ,pre-form))
                                       (*inside-contract-p* t))
                                   ,@(apply #'call-methods
                                            postcondition
                                            (if (eq gf #'make-instance)
                                                (list 'creation-invariant-error
                                                      :object reader-object
                                                      :description
                                                      `(invariant-description
                                                        ,reader-object))
                                                (list 'postcondition-error
                                                      :method (first primary))))
                                   (results)))
                               ,pre-form)
                          pre-form))
           #-:qpq-postcondition-checks
           (post-form pre-form)
           #+:qpq-invariant-checks
           (inv-form (if (and invariant-check
                              invariant
                              (not *inside-contract-p*))
                         `(if *check-invariants-p*
                              (multiple-value-prog1
                                  (progn
                                    (let ((*inside-contract-p* t))
                                      ,@(call-methods
                                         invariant
                                         'before-invariant-error
                                         :object `(or ,writer-object
                                                      ,reader-object)
                                         :method (first primary)
                                         :description
                                         `(invariant-description
                                           (class-of (or ,writer-object
                                                         ,reader-object)))))
                                    ,post-form)
                                (let ((*inside-contract-p* t))
                                  ,@(call-methods invariant
                                                  'after-invariant-error
                                                  :object `(or ,writer-object
                                                               ,reader-object)
                                                  :method (first primary)
                                                  :description
                                                  `(invariant-description
                                                    (class-of (or ,writer-object
                                                                  ,reader-object))))))
                              ,post-form)
                         post-form))
           #-:qpq-invariant-checks
           (inv-form post-form))
      ;; NOTE: This LET form is a workaround because there's no way to specify
      ;;       IGNORABLE in the right place, so we use the outer variables to
      ;;       bind the inner ones, then we can declare the inner ones as
      ;;       ignorable.
      `(let ((reader-object ,reader-object)
             (writer-object ,writer-object))
         (declare (ignorable reader-object writer-object))
         ,inv-form))))
