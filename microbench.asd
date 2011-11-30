(defsystem :microbench
  :depends-on (:alexandria :parenscript-classic :cl-who)
  :components
  ((:file "microbench")
   (:file "benchmarks")))
