(in-package #:quid-pro-quo.system)

(defsystem-connection quid-pro-quo.slot-value-invariants
  :requires (quid-pro-quo loom)
  :pathname #.(merge-pathnames "src/"
                               (asdf:system-source-directory :quid-pro-quo))
  :components ((:file "loom-slot-value-invariants")))
