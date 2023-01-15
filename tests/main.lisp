(defpackage pac/tests/main
  (:use :cl :fiveam :pac))
(in-package :pac/tests/main)
(def-suite pac/tests/main
  :description "Test my system")
(in-suite pac/tests/main)

(load "test.lisp")

(run!)
