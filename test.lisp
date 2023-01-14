(load "pac.lisp")
(ql:quickload :series)
(ql:quickload :fiveam)
(defpackage :pac (:use :cl :series :fiveam))
(in-package :pac)
(test imps 
  (is (= 3 (impfun0 '(1 1 1 -1 -1 -1))))
  (is (= 3 (impfun1 '(1 1 1 -1 -1 -1))))
  (is (= 3 (impfun2 '(1 1 1 -1 -1 -1))))
  )
(test assoc
  (is (= (+ 1 2) (+ 3 1)) "custom error message")
  )
