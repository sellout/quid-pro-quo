(in-package #:quid-pro-quo)

(defclass contracted-class (standard-class)
  ((invariants :initform () :initarg :invariants
               :reader direct-class-invariants)))

(defmethod validate-superclass
    ((class contracted-class) (superclass standard-class))
  t)

(defgeneric effective-class-invariants (class)
  (:method ((class contracted-class))
    (apply #'append
           (direct-class-invariants class)
           (mapcar #'effective-class-invariants
                   (class-direct-superclasses class))))
  (:method (class)
    (declare (ignore class))
    nil))

(defun check-effective-invariants (object)
  (loop for invariant in (effective-class-invariants (class-of object))
     unless (funcall invariant object)
     do (error 'creation-invariant-error
               :object object
               :description (documentation invariant 'function))))

(defun passes-class-invariants-p (object)
  (loop for invariant in (effective-class-invariants (class-of object))
     if (not (funcall invariant object))
     return nil
     finally (return t)))

(defun check-slot-type-invariants (object)
  (loop for slot in (class-slots (class-of object))
     unless (typep (slot-value object (slot-definition-name slot))
                   (slot-definition-type slot))
     do (error 'creation-invariant-error
               :object object
               :description (format nil "Slot ~A of ~A must be of type ~A"
                                    (slot-definition-name slot)
                                    object
                                    (slot-definition-type slot)))))

(defun passes-slot-type-invariants-p (object)
  (loop for slot in (class-slots (class-of object))
     unless (typep (slot-value object (slot-definition-name slot))
                   (slot-definition-type slot))
     return nil
     finally (return t)))

(defun passes-invariants-p (object)
  (and (passes-slot-type-invariants-p object)
       (passes-class-invariants-p object)))

(defun add-invariant (function-name lambda-list specializers lambda-body)
  (let* ((generic-function (ensure-generic-function
                            function-name
                            :lambda-list lambda-list
                            :method-combination '(contract)))
         (method-prototype (class-prototype (find-class 'standard-method)))
         (method-function (compile nil
                                   (make-method-lambda generic-function
                                                       method-prototype
                                                       `(lambda ,lambda-list
                                                          ,@lambda-body)
                                                       nil))))
    (add-method generic-function
                (make-instance 'standard-method
                               :qualifiers '(invariant)
                               :lambda-list lambda-list
                               :specializers specializers
                               :function method-function))))

(defun add-reader-invariant (reader class)
  (add-invariant reader '(object) (list class) '((passes-invariants-p object))))

(defun add-writer-invariant (writer class)
  (add-invariant writer
                 '(new-value object)
                 (list (find-class t) class)
                 '((declare (ignore new-value))
                   (passes-invariants-p object))))

(defun all-direct-slots (class)
  (apply #'append
         (class-direct-slots class)
         (mapcar #'all-direct-slots (class-direct-superclasses class))))

(defmethod initialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants))
  (let ((slots (all-direct-slots instance)))
    (mapc (lambda (reader) (add-reader-invariant reader instance))
          (reduce #'append (mapcar #'slot-definition-readers slots)))
    (mapc (lambda (writer) (add-writer-invariant writer instance))
          (reduce #'append (mapcar #'slot-definition-writers slots)))))

(defmethod reinitialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants)))

(defmethod make-instance ((class contracted-class) &rest initargs)
  (declare (ignorable initargs)) ; NOTE: not ignorable, but CCL complains
  (let ((object (call-next-method)))
    (check-slot-type-invariants object)
    (check-effective-invariants object)
    object))
