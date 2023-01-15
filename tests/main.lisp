(asdf:load-system :pac)
(asdf:load-system :pac/tests)
(defpackage pac/tests
  (:use :cl :pac :fiveam))
(load "tests/test")

(in-package :pac/tests)
(run!)

