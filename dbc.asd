(defpackage dbc-system
  (:use #:cl #:asdf))

(in-package #:dbc-system)

(defsystem dbc
  :author "Matthias Hölzl <tc@gauss.muc.de>"
  :maintainer "Greg Pfeil <greg@technomadic.org>"
  :license "Public Domain"
  :components ((:file "dbc"))
  :in-order-to ((test-op (load-op dbc-tests))))

(defmethod operation-done-p
    ((op test-op) (c (eql (find-system :dbc))))
  (values nil))

(defsystem dbc-tests
  :author "Matthias Hölzl <tc@gauss.muc.de>"
  :maintainer "Greg Pfeil <greg@technomadic.org>"
  :depends-on (dbc)
  :components ((:file "dbc-test")))
