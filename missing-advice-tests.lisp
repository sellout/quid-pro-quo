(in-package #:quid-pro-quo-test)

(def-suite missing-advice-tests :in tests)

(in-suite missing-advice-tests)

(defun add (&rest addends)
  (apply #'+ addends))

(test should-warn-and-fail-to-create-contract
  (signals warning
    (defrequirement add (&rest addends)
      "all args < 10"
      (every (lambda (n) (< n 10)) addends))))

(handler-bind ((warning #'muffle-warning))
  (defguarantee add (&rest addends)
    "result < 20"
    (declare (ignore addends))
    (< (results) 20)))

(test advice-should-pass-regardless
  (is (= 18 (add 9 9)))
  (is (= 18 (add 3 3 3 3 3 3)))
  (is (= 19 (add 9 10)))
  (is (= 27 (add 9 9 9))))
