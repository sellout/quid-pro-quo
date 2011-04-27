(in-package #:quid-pro-quo.system)

(defsystem-connection quid-pro-quo.slot-value-invariants
  :requires (quid-pro-quo loom)
  :components ((:file "slot-value-invariants")))
