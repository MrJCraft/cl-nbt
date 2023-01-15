(asdf:load-asd "pac.asd")
(defpackage pac/tests
  (:use :cl :pac :fiveam))
(load "tests/test")

(in-package :pac/tests)
(run!)

