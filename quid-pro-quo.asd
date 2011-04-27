(defpackage quid-pro-quo.system
  (:use #:cl #:asdf))

(in-package #:quid-pro-quo.system)

(defsystem quid-pro-quo
  :author "Matthias Hoelzl <tc@gauss.muc.de>"
  :maintainer "Greg Pfeil <greg@technomadic.org>"
  :license "Public Domain"
  :depends-on (closer-mop asdf-system-connections)
  :pathname "src/"
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "method-combination" :depends-on ("package"))
               (:file "metaclass" :depends-on ("method-combination"))
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
  :components ((:file "quid-pro-quo-test")))
