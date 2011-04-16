(in-package #:dbc)

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
     if (not (funcall invariant object))
     do (error 'creation-invariant-error
               :object object
               :description (documentation invariant 'function))))

(defun passes-class-invariants-p (object)
  (loop for invariant in (effective-class-invariants (class-of object))
     if (not (funcall invariant object))
     return nil
     finally (return t)))

(defun add-reader-invariant (reader class)
  (add-method (ensure-generic-function reader
                                       :lambda-list '(object)
                                       :method-combination '(contract))
              (make-instance 'standard-method
                             :qualifiers '(:invariant)
                             :lambda-list '(object)
                             :specializers (list class)
                             :function #'passes-class-invariants-p)))

(defun add-writer-invariant (writer class)
  (add-method (ensure-generic-function writer
                                       :lambda-list '(new-value object)
                                       :method-combination '(contract))
              (make-instance 'standard-method
                             :qualifiers '(:invariant)
                             :lambda-list '(new-value object)
                             :specializers (list (find-class t) class)
                             :function (lambda (new-value object)
                                         (declare (ignore new-value))
                                         (passes-class-invariants-p object)))))

(defun all-direct-slots (class)
  (apply #'append
         (class-direct-slots class)
         (mapcar #'all-direct-slots (class-direct-superclasses class))))

(defmethod initialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants))
  ;; FIXME: need to do this for all slots, not just direct slots
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
    (check-effective-invariants object)
    object))
