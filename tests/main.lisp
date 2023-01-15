(push "test/" asdf:*central-registry*)
(defpackage pac/tests
  (:use :cl :pac :fiveam))
(load "tests/test")

(in-package :pac/tests)
(run!)

