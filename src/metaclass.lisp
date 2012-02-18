(in-package #:quid-pro-quo)

(defclass contracted-class (standard-class)
  ((invariants :initform () :initarg :invariants
               :reader direct-class-invariants)
   (invariant-descriptions :initform ()))
  (:documentation
   "Use this as the metaclass for any classes you want to add invariants to."))

(defmethod documentation ((x contracted-class) (doc-type (eql 'type)))
  "Appends the invariant information to the usual documentation."
  (format nil "~@[~A~]~@[~&contract:~{~&* ~A~}~]"
          (call-next-method)
          (append (loop for slot in (class-slots x)
                     unless (eq (slot-definition-type slot) t)
                     collect (format nil "~A is of type ~A"
                                     (slot-definition-name slot)
                                     (slot-definition-type slot)))
                  (mapcar (lambda (function body)
                            (or (documentation function t) body))
                          (class-invariants x)
                          (class-invariant-descriptions x)))))

(defmethod documentation ((x contracted-class) (doc-type (eql 't)))
  (documentation x 'type))

(defmethod validate-superclass
    ((class contracted-class) (superclass standard-class))
  t)

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

(defmethod initialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants)
        (slot-value instance 'invariant-descriptions)
        (mapcar #'cddr invariants))
  (mapc (lambda (function) (funcall function instance))
        *invariant-initializers*))

(defmethod reinitialize-instance :after
    ((instance contracted-class) &key invariants &allow-other-keys)
  (setf (slot-value instance 'invariants) (mapcar #'eval invariants)
        (slot-value instance 'invariant-descriptions)
        (mapcar #'cddr invariants)))

;; NOTE: Ideally this would be an invariant on MAKE-INSTANCE, but that would
;;       also get checked _before_ creation. So instead, we make it a
;;       postcondition, but special-case it in the method-combination to error
;;       as an invariant.

(ensure-generic-function 'make-instance :method-combination '(contract))

(defmethod make-instance :ensure ((class contracted-class) &rest initargs)
  (declare (ignorable initargs)) ; NOTE: not ignorable, but CCL complains
  (passes-invariants-p (results)))
