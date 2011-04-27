(defpackage qpq-stack-example
  (:use #:cl) ; NOTE: would normally use QPQ, but qualification helps illustrate
  (:export #:stack
           #:size #:capacity
           #:item #:emptyp #:fullp
           #:put #:unput))

(in-package #:qpq-stack-example)

(defclass stack ()
  ((size :initform 0 :reader size :type (integer 0))
   (capacity :initarg :capacity :reader capacity :type (integer 0))
   representation)
  (:documentation "A simple stack implementation for a contract example.")
  (:metaclass quid-pro-quo:contracted-class)
  (:invariants (lambda (object) (<= (size object) (capacity object)))
               (lambda (object) (eq (emptyp object) (= (size object) 0)))))

(defgeneric initialize-instance (class &key &allow-other-keys)
  (:method-combination quid-pro-quo:contract)
  (:method :require "CAPACITY is non-negative"
           ((instance stack) &key capacity &allow-other-keys)
    (>= capacity 0))
  (:method :after ((instance stack) &key capacity &allow-other-keys)
    (setf (slot-value instance 'representation) (make-array capacity)))
  (:method :ensure "CAPACITY was set and stack is empty"
           ((instance stack) &key capacity &allow-other-keys)
    (and (= (capacity instance) capacity)
         (emptyp instance))))

(defgeneric item (object)
  (:method-combination quid-pro-quo:contract)
  (:method :require "stack not empty" ((object stack))
    (not (emptyp object)))
  (:method ((object stack))
    (aref (slot-value object 'representation) (1- (size object)))))

(defgeneric emptyp (object)
  (:method-combination quid-pro-quo:contract)
  (:method ((object stack))
    (= (size object) 0))
  (:method :ensure "result matches definition" ((object stack))
    (eq (quid-pro-quo:results) (= (size object) 0))))

(defgeneric fullp (object)
  (:method-combination quid-pro-quo:contract)
  (:method ((object stack))
    (= (size object) (capacity object)))
  (:method :ensure "result matches definition" ((object stack))
    (eq (quid-pro-quo:results) (= (size object) (capacity object)))))

(defgeneric put (item object)
  (:method-combination quid-pro-quo:contract)
  (:method :require "stack not full" (item (object stack))
    (declare (ignore item))
    (not (fullp object)))
  (:method (item (object stack))
    (setf (aref (slot-value object 'representation) (size object)) item)
    (incf (slot-value object 'size))
    (values))
  (:method :ensure "size increased, stack not empty, & ITEM added to top"
           (item (object stack))
    (and (= (size object) (1+ (quid-pro-quo:old (size object))))
         (not (emptyp object))
         (eq (item object) item))))

(defgeneric unput (object)
  (:method-combination quid-pro-quo:contract)
  (:method :require "stack not empty" ((object stack))
    (not (emptyp object)))
  (:method ((object stack))
    (decf (slot-value object 'size)))
  (:method :ensure "size decreased & stack not full" ((object stack))
    (and (= (size object) (1- (quid-pro-quo:old (size object))))
         (not (fullp object)))))
