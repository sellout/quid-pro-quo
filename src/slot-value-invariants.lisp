(in-package #:quid-pro-quo)

;;; NOTE: This file assumes that LOOM has been loaded.

(defun add-slot-value-invariants (class)
  (add-invariant 'loom:slot-value
                 '(object slot-name)
                 (list class (find-class t))
                 '((declare (ignore slot-name))
                   (passes-invariants-p object)))
  (add-invariant '(setf loom:slot-value)
                 '(new-value object slot-name)
                 (list (find-class t) class (find-class t))
                 '((declare (ignore new-value slot-name))
                   (passes-invariants-p object))))

(push #'add-slot-value-invariants *invariant-initializers*)
