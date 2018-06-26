(in-package #:quid-pro-quo)

(defclass contracted-class (standard-class)
  ((invariants :initform () :initarg :invariants
               :reader direct-class-invariants)
   (invariant-descriptions :initform ()))
  (:documentation
   "This is the metaclass for any classes you want to add invariants to."))

(defclass funcallable-contracted-class
    (contracted-class funcallable-standard-class)
  ()
  (:documentation
   "This is the metaclass for any funcallable classes you want to add invariants
    to."))

(defun invariant-description (class)
  (let ((description (append (loop for slot in (class-slots class)
                                unless (eq (slot-definition-type slot) t)
                                collect (format nil "~A is of type ~A"
                                                (slot-definition-name slot)
                                                (slot-definition-type slot)))
                             (class-invariant-descriptions class))))
    (when description
      (format nil "~{~&* ~A~}" description))))

(defmethod documentation ((x contracted-class) (doc-type (eql 'type)))
  "Appends the invariant information to the usual documentation."
  (format nil "~@[~A~]~@[~&contract:~%~A~]"
          (call-next-method) (invariant-description x)))

(defmethod documentation ((x contracted-class) (doc-type (eql 't)))
  (documentation x 'type))

(defmethod validate-superclass
    ((class contracted-class) (superclass standard-class))
  (and (member (class-of class)
               (list (find-class 'contracted-class)
                     (find-class 'funcallable-contracted-class)))
       (member (class-of superclass)
               (list (find-class 'standard-class)
                     (find-class 'funcallable-standard-class)
                     (find-class 'contracted-class)
                     (find-class 'funcallable-contracted-class)))))

#-allegro
(defmethod ensure-class-using-class :around
    (class name &rest args &key direct-superclasses metaclass &allow-other-keys)
  "This ensures that any subclass of a CONTRACTED-CLASS is also treated as a
   CONTRACTED-CLASS (assuming the METACLASS and all DIRECT-SUPERCLASSES are
   compatible with CONTRACTED-CLASS). This helps us maintain contracts without
   the subclasser having to know about them.

   NB: This doesn't work on forward-referenced classes."
  (flet ((get-class (class) (if (classp class) class (find-class class nil))))
    (let ((contracted-class (find-class 'contracted-class)))
      (if (and ;; skip if it's already specified as a CONTRACTED-CLASS
               (not (and metaclass (subtypep metaclass contracted-class)))
               ;; skip if there is no contracted superclass
               (some (lambda (sc) (typep (get-class sc) contracted-class))
                     direct-superclasses)
               (let ((contracted-prototype (class-prototype contracted-class)))
                 (and ;; skip if the metaclass is not compatible
                      (validate-superclass contracted-prototype
                                           (class-prototype
                                            (get-class (or metaclass
                                                           'standard-class))))
                      ;; skip if some superclass is not compatible
                      (every (lambda (sc)
                               (validate-superclass contracted-prototype
                                                    (get-class sc)))
                             direct-superclasses))))
          (progn
            ;;; NOTE: We SETF here instead of just CONSing the new value onto
            ;;;       ARGS because CMUCL errors if there are two method
            ;;;       combinations, even though that is legal CL.
            (setf (getf args :metaclass)
                  (if (subtypep metaclass 'funcallable-standard-class)
                      'funcallable-contracted-class
                      'contracted-class))
            (apply #'call-next-method class name args))
          (call-next-method)))))

(defgeneric class-invariants (class)
  (:method ((class contracted-class))
    (apply #'append
           (direct-class-invariants class)
           (mapcar #'class-invariants (class-direct-superclasses class))))
  (:method (class)
    (declare (ignore class))
    nil))

(defgeneric class-invariant-descriptions (class)
  (:method ((class contracted-class))
    (apply #'append
           (slot-value class 'invariant-descriptions)
           (mapcar #'class-invariant-descriptions
                   (class-direct-superclasses class))))
  (:method (class)
    (declare (ignore class))
    nil))

(defun passes-class-invariants-p (object)
  (loop for invariant in (class-invariants (class-of object))
     if (not (funcall invariant object))
     return nil
     finally (return t)))

(defun passes-slot-type-invariants-p (object)
  (loop for slot in (class-slots (class-of object))
     unless (implies (slot-boundp object (slot-definition-name slot))
                     (typep (slot-value object (slot-definition-name slot))
                            (slot-definition-type slot)))
     return nil
     finally (return t)))

(defun passes-invariants-p (object)
  "This checks that an instance passes all the class invariants."
  (and (passes-slot-type-invariants-p object)
       (passes-class-invariants-p object)))

(defun add-invariant
    (function-name description lambda-list specializers lambda-body)
  "Adds an invariant to the provided generic function."
  #+qpq-invariant-checks-disabled
  (declare (ignore function-name description lambda-list specializers
                   lambda-body))
  #+qpq-invariant-checks-disabled
  (values)
  #-qpq-invariant-checks-disabled
  (let* ((generic-function (ensure-contracted-function function-name
                                                       lambda-list))
         (method-prototype (class-prototype (find-class 'standard-method)))
         (method-lambda (make-method-lambda generic-function
                                            method-prototype
                                            `(lambda ,lambda-list
                                               ,@(when description
                                                   (list description))
                                               ,@lambda-body)
                                            nil)))
    (add-method generic-function
                (make-instance 'standard-method
                               :qualifiers (list 'invariant description)
                               :lambda-list lambda-list
                               :specializers specializers
                               :function (compile nil method-lambda)))))

(defun add-reader-invariant (reader class)
  (add-invariant reader
                 (invariant-description class)
                 '(object)
                 (list class)
                 '((passes-invariants-p object))))

(defun add-writer-invariant (writer class)
  (add-invariant writer
                 (invariant-description class)
                 '(new-value object)
                 (list (find-class t) class)
                 '((declare (ignore new-value))
                   (passes-invariants-p object))))

(defun all-direct-slots (class)
  (append (class-direct-slots class)
          (mappend #'all-direct-slots (class-direct-superclasses class))))

(defun add-accessor-invariants (class)
  (let ((slots (all-direct-slots class)))
    (mapc (lambda (reader) (add-reader-invariant reader class))
          (reduce #'append (mapcar #'slot-definition-readers slots)))
    (mapc (lambda (writer) (add-writer-invariant writer class))
          (reduce #'append (mapcar #'slot-definition-writers slots)))))

(defvar *invariant-initializers* (list #'add-accessor-invariants)
  "This is a list of functions that add invariants to some methods on a class.
   Each function must take the class as an argument. The return value is
   ignored.")

(defun initialize-invariants (instance invariants)
  (labels ((create-description (body)
             (multiple-value-bind (remaining-forms declarations doc-string)
                 (parse-body body :documentation t)
               (declare (ignore declarations))
               (or doc-string (description<-remaining-forms remaining-forms))))
           (function-description (fn)
             (format nil "~A~@[ (~A)~]" fn (documentation fn 'function))))
    (setf (slot-value instance 'invariants)
          (mapcar #'eval (remove-if (rcurry #'typep 'string) invariants))
          (slot-value instance 'invariant-descriptions)
          (mapcar (lambda (invariant)
                    (typecase invariant
                      (list (if (eq 'function (car invariant))
                                (function-description (cadr invariant))
                                (create-description (cddr invariant))))
                      ((or function symbol) (function-description invariant))
                      (string invariant)))
                  invariants))))

(defmethod initialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (initialize-invariants instance invariants))

;; NOTE: This is done in MAKE-INSTANCE rather than INITIALIZE-INSTANCE because
;;       the class needs to be finalized before we can loop over the slots.
#-qpq-invariant-checks-disabled
(defmethod make-instance :after
    ((class contracted-class) &key &allow-other-keys)
  (mapc (lambda (function) (funcall function class))
        *invariant-initializers*))

#-qpq-invariant-checks-disabled
(defmethod reinitialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (initialize-invariants instance invariants))

;; NOTE: Ideally this would be an invariant on MAKE-INSTANCE, but that would
;;       also get checked _before_ creation. So instead, we make it a
;;       postcondition, but special-case it in the method-combination to error
;;       as an invariant.

;; FIXME: Allegro, CLISP, and LispWorks explode if we execute the following
;;        code, so we basically skip creation invariants on those.
#-(or allegro clisp lispworks sbcl)
(defguarantee make-instance ((class contracted-class) &key &allow-other-keys)
  (passes-invariants-p (results)))
