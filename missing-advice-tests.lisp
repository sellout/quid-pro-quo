(in-package #:quid-pro-quo-test)

(def-suite missing-advice-tests :in tests)

(in-suite missing-advice-tests)

(defun add (&rest addends)
  (apply #'+ addends))

(test should-warn-and-fail-to-create-contract
  (signals warning
    (defcontract add :require "all args < 10" (&rest addends)
      (every (lambda (n) (< n 10)) addends))))

(handler-bind ((warning #'muffle-warning))
  (defcontract add :ensure "result < 20" (&rest addends)
    (declare (ignore addends))
    (< (results) 20)))

(test advice-should-pass-regardless
  (is (= 18 (add 9 9)))
  (is (= 18 (add 3 3 3 3 3 3)))
  (is (= 19 (add 9 10)))
  (is (= 27 (add 9 9 9))))
