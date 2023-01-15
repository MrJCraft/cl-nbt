(in-package :pac/test/main)
(def-suite imps
  :description "Test my system")
(in-suite imps)
(test imps 
  (is (= 3 (impfun0 '(1 1 1 -1 -1 -1))))
  (is (= 3 (impfun1 '(1 1 1 -1 -1 -1))))
  (is (= 3 (impfun2 '(1 1 1 -1 -1 -1))))
  )
(test assoc
  (is (= (+ 1 2) (+ 3 1)) "custom error message")
  )
