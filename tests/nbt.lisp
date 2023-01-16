(in-package #:tests)

(def-suite :tnbt
  :description "regresssion testing for cl-nbt main functions")

(in-suite :tnbt)

(test nbt-write
  (is
   (equal (coerce (gunzip "tests/tuffsand.schem") 'list)
          (nbt-write (nbt-read (coerce (gunzip "tests/tuffsand.schem") 'list)) "test.txt")
          )
   )
  )


