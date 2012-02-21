(in-package #:quid-pro-quo-test)

(def-suite advice-tests :in tests)

(in-suite advice-tests)

(defun add (&rest addends)
  (apply #'+ addends))

(defcontract add :require "all args < 10" (&rest addends)
  (every (lambda (n) (< n 10)) addends))

(defcontract add :ensure "result < 20" (&rest addends)
  (declare (ignore addends))
  (< (results) 20))

(test advice-should-pass-contract
  (is (= 18 (add 9 9)))
  (is (= 18 (add 3 3 3 3 3 3))))

(test advice-should-fail-precondition
  (signals precondition-error
    (add 9 10)))

(test advice-should-fail-postcondition
  (signals postcondition-error
    (add 9 9 9)))
