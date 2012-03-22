(defpackage quid-pro-quo.system
  (:use #:cl #:asdf))

(in-package #:quid-pro-quo.system)

(defsystem quid-pro-quo
  :description "A contract programming library for Common Lisp in the style of
                Eiffel's Design by Contract."
  :long-description "See README.md"
  :author "Matthias Hoelzl <tc@gauss.muc.de>"
  :maintainer "Greg Pfeil <greg@technomadic.org>"
  :license "Public Domain"
  :depends-on (closer-mop
               method-combination-utilities
               asdf-system-connections
               alexandria)
  :pathname "src/"
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "method-combination" :depends-on ("package"))
               (:file "metaclass" :depends-on ("method-combination"))
               (:file #+allegro "acl-fwrap"
                      #+ccl "ccl-advice"
                      #-(or allegro ccl) "missing-advice"
                      :depends-on ("package"))
               (:file "macros" :depends-on (#+allegro "acl-fwrap"
                                            #+ccl "ccl-advice"
                                            #-(or allegro ccl)
                                            "missing-advice"))
               (:file "system-connections" :depends-on ("metaclass")
                      :description "Enumerates conditionally-loaded files. Look
                                    here for the other files that may be loaded
                                    with this system."))
  :in-order-to ((test-op (load-op quid-pro-quo-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :quid-pro-quo-test)
                             (intern "TESTS" :quid-pro-quo-test))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :quid-pro-quo))))
  (values nil))

(defsystem quid-pro-quo-tests
  :author "Matthias Hoelzl <tc@gauss.muc.de>"
  :maintainer "Greg Pfeil <greg@technomadic.org>"
  :depends-on (quid-pro-quo fiveam)
  :components ((:file "quid-pro-quo-test")
               (:file #+(or allegro ccl) "advice-tests"
                      #-(or allegro ccl) "missing-advice-tests"
                      :depends-on ("quid-pro-quo-test"))))
