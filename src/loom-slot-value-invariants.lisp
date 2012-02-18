(in-package #:quid-pro-quo)

;;; NOTE: This file assumes that LOOM has been loaded.

(defun add-loom-slot-value-invariants (class)
  "With LOOM, SLOT-VALUE is a generic function, which means we can test
   invariants on it, rather than just at the level of accessors. This isn't
   fool-proof, as CL:SLOT-VALUE is still accessible, but it gets us one step
   closer."
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

(push #'add-loom-slot-value-invariants *invariant-initializers*)
